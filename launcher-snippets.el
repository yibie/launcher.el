;;; launcher-snippets.el --- Org snippets with placeholders for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Snippets for launcher: each heading of an Org file is a snippet whose
;; body is copied to the kill ring, with placeholders such as {clipboard},
;; {date}, {input:PROMPT} and {choice:PROMPT|A,B} filled first.  Add
;; (launcher-snippets-tool "t") to `launcher-tools'.  The placeholders are
;; described in `launcher-snippets-expand'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'url-util)
(require 'time-date)

(defvar launcher-after-copy-hook)
(declare-function org-id-uuid "org-id")
(declare-function json-encode-string "json")

(defcustom launcher-snippets-file (locate-user-emacs-file "snippets.org")
  "Org file holding the snippets `launcher-snippets-tool' offers.
Each heading with a non-empty body is a snippet: the heading is its name,
the body (up to the next heading, blank edge lines trimmed) its text.  The
file is read each time launcher opens.  Placeholders in the text are listed
in `launcher-snippets-expand'."
  :type 'file
  :group 'launcher)

(defcustom launcher-snippets-weekday-names nil
  "Names {day} uses for Sunday through Saturday, or nil for the locale's.
A vector of seven strings, such as [\"星期日\" \"星期一\" ...].
An explicit format=\"...\" option on {day} ignores this."
  :type '(choice (const :tag "Locale (%A)" nil) (vector string string string string string string string))
  :group 'launcher)

(defun launcher-snippets--read (&optional file)
  "Return the snippets in FILE (`launcher-snippets-file') as candidates.
Nil when the file does not exist."
  (let ((file (or file launcher-snippets-file)) snippets)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+[ \t]+\\(.*?\\)[ \t]*$" nil t)
          (let* ((name (match-string-no-properties 1))
                 (start (min (1+ (match-end 0)) (point-max)))
                 (end (save-excursion
                        (if (re-search-forward "^\\*+[ \t]" nil t) (pos-bol) (point-max))))
                 ;; A property drawer under the heading is metadata, not snippet text.
                 (body (string-trim
                        (replace-regexp-in-string
                         "\\`[ \t\n]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\\(?:\n\\|\\'\\)" ""
                         (buffer-substring-no-properties start end))
                        "\\(?:[ \t]*\n\\)+" "\\(?:\n[ \t]*\\)+")))
            (unless (string-blank-p body)
              (push (list :id name :title name
                          :subtitle (truncate-string-to-width
                                     (car (split-string body "\n")) 60 nil nil "…")
                          :data body)
                    snippets))))))
    (nreverse snippets)))

(defconst launcher-snippets--placeholder-re
  (concat "{\\(clipboard\\|datetime\\|date\\|time\\|day\\|uuid"
          "\\|input\\|multi\\|choice\\|snippet\\|optional\\)"
          "\\(\\(?:[ :][^{}\n]*\\)?\\)}")
  "Match a snippet placeholder: its name (1), then what follows the name (2).")

(defconst launcher-snippets--optional-end "{/optional}"
  "Closes the section a {optional:PROMPT} placeholder opens.")

(defvar launcher-snippets--answers nil
  "What this expansion has already asked, as (KEY . ANSWER).")

(defvar launcher-snippets--stack nil
  "Names of the snippets being expanded, to refuse a {snippet:...} cycle.")

(defvar launcher-snippets--read-input nil
  "The function answering {input:...}, called with a prompt and initial input.")

(defvar-keymap launcher-snippets-multi-map
  :doc "Minibuffer keys of a {multi:...} answer: RET finishes, C-j adds a line."
  :parent minibuffer-local-map
  "C-j" #'newline
  "S-<return>" #'newline)

(defvar launcher-snippets--last-output nil
  "What the last snippet put on the clipboard.")

(defvar launcher-snippets--last-copied nil
  "What {clipboard} meant when the last snippet ran.")

(defun launcher-snippets--clipboard ()
  "Return what the user last copied, for {clipboard}.
A snippet's result also lands on the clipboard so it can be pasted; treating
that as a copy would feed one snippet's output into the next.  So while the
clipboard still holds the last result, this is what it held before."
  (let ((current (or (gui-get-selection 'CLIPBOARD) "")))
    (if (equal current launcher-snippets--last-output)
        (or launcher-snippets--last-copied "")
      current)))

(defun launcher-snippets--shift-time (time offset)
  "Return TIME moved by OFFSET, words like +1d or -2h separated by spaces.
Units are m minutes, h hours, d days, w weeks, M months and y years; months
and years follow the calendar."
  (let ((decoded (decode-time time)) (case-fold-search nil))
    (dolist (word (split-string offset))
      (unless (string-match "\\`\\([+-][0-9]+\\)\\([mhdwMy]\\)\\'" word)
        (user-error "Snippet: unknown time offset %s" word))
      (let ((n (string-to-number (match-string 1 word))))
        (setq decoded
              (decoded-time-add
               decoded
               (pcase (match-string 2 word)
                 ("m" (make-decoded-time :minute n))
                 ("h" (make-decoded-time :hour n))
                 ("d" (make-decoded-time :day n))
                 ("w" (make-decoded-time :day (* 7 n)))
                 ("M" (make-decoded-time :month n))
                 ("y" (make-decoded-time :year n)))))))
    (setf (decoded-time-dst decoded) -1)
    (encode-time decoded)))

(defun launcher-snippets--options (text)
  "Return the KEY=VALUE options in TEXT as an alist; VALUE may be double-quoted."
  (let ((pos 0) options)
    (while (string-match "\\([a-z]+\\)=\\(?:\"\\([^\"]*\\)\"\\|\\([^ ]+\\)\\)" text pos)
      (push (cons (match-string 1 text) (or (match-string 2 text) (match-string 3 text)))
            options)
      (setq pos (match-end 0)))
    options))

(defun launcher-snippets--auto (name options)
  "Return the value of the automatic placeholder NAME with its OPTIONS text."
  (let* ((options (launcher-snippets--options options))
         (offset (cdr (assoc "offset" options)))
         (format (cdr (assoc "format" options)))
         (time (and offset (launcher-snippets--shift-time (current-time) offset))))
    (pcase name
      ("clipboard" (launcher-snippets--clipboard))
      ("date" (format-time-string (or format "%Y-%m-%d") time))
      ("time" (format-time-string (or format "%H:%M") time))
      ("datetime" (format-time-string (or format "%Y-%m-%d %H:%M") time))
      ("day" (cond (format (format-time-string format time))
                   (launcher-snippets-weekday-names
                    (aref launcher-snippets-weekday-names
                          (decoded-time-weekday (decode-time time))))
                   (t (format-time-string "%A" time))))
      ("uuid" (require 'org-id) (org-id-uuid)))))

(defun launcher-snippets--default (default)
  "Return the initial answer DEFAULT stands for, or nil when there is none.
An automatic placeholder without braces, options included, stands for its
value; anything else for itself."
  (when default
    (if (string-match "\\`\\(clipboard\\|datetime\\|date\\|time\\|day\\|uuid\\)\\(?: \\(.*\\)\\)?\\'"
                      default)
        (launcher-snippets--auto (match-string 1 default) (or (match-string 2 default) ""))
      default)))

(defun launcher-snippets--modify (value modifier)
  "Return VALUE transformed by MODIFIER."
  (pcase modifier
    ("trim" (string-trim value))
    ("uppercase" (upcase value))
    ("lowercase" (downcase value))
    ("capitalize" (capitalize value))
    ("oneline" (string-join (split-string (string-trim value) "[ \t]*\n[ \t]*" t) " "))
    ("quote" (mapconcat (lambda (line) (concat "> " line)) (split-string value "\n") "\n"))
    ("percent-encode" (url-hexify-string value))
    ("json-stringify" (require 'json) (json-encode-string value))
    (_ (user-error "Snippet: unknown modifier %s" modifier))))

(defun launcher-snippets--ask (key ask)
  "Return the answer already given for KEY, or what calling ASK returns."
  (if-let* ((known (assoc key launcher-snippets--answers)))
      (cdr known)
    (let ((answer (funcall ask)))
      (push (cons key answer) launcher-snippets--answers)
      answer)))

(defun launcher-snippets--choose (prompt options)
  "Ask PROMPT, one of OPTIONS in their written order; the first is the default."
  (completing-read (format "%s: " prompt)
                   (lambda (string pred action)
                     (if (eq action 'metadata)
                         '(metadata (display-sort-function . identity))
                       (complete-with-action action options string pred)))
                   nil t nil t (car options)))

(defun launcher-snippets--include (name)
  "Return the expansion of the snippet called NAME."
  (when (member name launcher-snippets--stack)
    (user-error "Snippet: %s includes itself" name))
  (let ((snippet (cl-find name (launcher-snippets--read)
                          :key (lambda (s) (plist-get s :title)) :test #'equal)))
    (unless snippet (user-error "Snippet: no snippet named %s" name))
    (let ((launcher-snippets--stack (cons name launcher-snippets--stack)))
      (launcher-snippets--fill (plist-get snippet :data)))))

(defun launcher-snippets--render (name rest)
  "Return the value of placeholder NAME; REST is the text after its name."
  (pcase-let* ((`(,head . ,modifiers) (split-string rest " [|] "))
               (argument (string-remove-prefix ":" head))
               (`(,prompt ,default)
                (if (string-match "|" argument)
                    (list (substring argument 0 (match-beginning 0))
                          (substring argument (match-end 0)))
                  (list argument nil)))
               (value
                (pcase name
                  ("input"
                   (launcher-snippets--ask
                    prompt
                    (lambda ()
                      (funcall launcher-snippets--read-input (format "%s: " prompt)
                               (launcher-snippets--default default)))))
                  ("multi"
                   (launcher-snippets--ask
                    prompt
                    (lambda ()
                      (read-from-minibuffer (format "%s (C-j for newline): " prompt)
                                            (launcher-snippets--default default)
                                            launcher-snippets-multi-map nil t))))
                  ("choice"
                   (launcher-snippets--ask
                    prompt
                    (lambda ()
                      (launcher-snippets--choose
                       prompt
                       (or (split-string (or default "") "[ \t]*[,，][ \t]*" t)
                           (user-error "Snippet: %s lists no options" prompt))))))
                  ("snippet" (launcher-snippets--include argument))
                  (_ (launcher-snippets--auto name head)))))
    (seq-reduce #'launcher-snippets--modify modifiers value)))

(defun launcher-snippets--fill (text)
  "Return TEXT with its placeholders filled, left to right.
Filled values are not scanned again, so a clipboard holding {date} stays so."
  (let ((pos 0) (case-fold-search nil) parts)
    (while (string-match launcher-snippets--placeholder-re text pos)
      (let ((start (match-beginning 0)) (end (match-end 0))
            (name (match-string 1 text)) (rest (match-string 2 text)))
        (push (substring text pos start) parts)
        (if (not (equal name "optional"))
            (progn (push (launcher-snippets--render name rest) parts)
                   (setq pos end))
          (let* ((prompt (string-remove-prefix ":" rest))
                 (close (or (string-search launcher-snippets--optional-end text end)
                            (user-error "Snippet: {optional:%s} lacks {/optional}" prompt)))
                 (after (+ close (length launcher-snippets--optional-end))))
            (when (launcher-snippets--ask
                   (cons 'optional prompt)
                   (lambda () (y-or-n-p (format "%s？" prompt))))
              (push (launcher-snippets--fill
                     (string-remove-prefix "\n" (substring text end close)))
                    parts))
            ;; A marker alone on its line goes with its newline, leaving no blank line.
            (setq pos (if (and (< after (length text)) (eq (aref text after) ?\n))
                          (1+ after)
                        after))))))
    (push (substring text pos) parts)
    (apply #'concat (nreverse parts))))

;;;###autoload
(defun launcher-snippets-expand (text &optional read-input)
  "Return TEXT with its placeholders filled.

Filled without asking:
  {clipboard}   what the user last copied, not the last snippet's result
  {date}        2026-01-31       {time}  14:05
  {datetime}    2026-01-31 14:05 {day}   Saturday (see
                                          `launcher-snippets-weekday-names')
  {uuid}        a random UUID
{date}, {time}, {datetime} and {day} take offset=\"+1d -2h\" (units m h d w M
y) and format=\"%Y/%m/%d\" (`format-time-string').

Asked once per prompt, in the order written:
  {input:PROMPT}         one line, read by READ-INPUT (`read-string'), which
                         gets the prompt and an initial input
  {input:PROMPT|DEFAULT} starts from DEFAULT, to accept with RET or edit;
                         an automatic placeholder without braces stands for
                         its value: {input:Due|date offset=\"+7d\"}
  {multi:PROMPT|DEFAULT} several lines; C-j adds a line, RET finishes
  {choice:PROMPT|A,B,C}  one of the options, the first by default
  {optional:PROMPT}...{/optional}  keep the section only on yes; not nestable

  {snippet:NAME}         another snippet from the same file, sharing answers

Any placeholder but {optional:...} ends in modifiers applied in order:
{clipboard | trim | uppercase}.  See `launcher-snippets--modify'.  Other
braces are left alone.  Answers are not recorded in a minibuffer history: a
prefilled clipboard may be a secret."
  (let ((launcher-snippets--answers nil)
        (launcher-snippets--stack nil)
        (launcher-snippets--read-input
         (or read-input (lambda (prompt initial) (read-string prompt initial t)))))
    (launcher-snippets--fill text)))

(defun launcher-snippets-submit (candidate)
  "Fill CANDIDATE's snippet once launcher closes and copy it to the kill ring.
Placeholders may prompt, so the work is returned as a function for
`launcher' to call after its minibuffer has closed.  A broken snippet
copies nothing and reports its error."
  (let ((name (plist-get candidate :title)) (text (plist-get candidate :data)))
    (lambda ()
      (condition-case err
          (let ((copied (launcher-snippets--clipboard))
                (output (launcher-snippets-expand text)))
            (setq launcher-snippets--last-copied copied
                  launcher-snippets--last-output output)
            (kill-new output)
            (message "Copied snippet: %s" name)
            (run-hooks 'launcher-after-copy-hook))
        (error (message "%s" (error-message-string err))))
      nil)))

;;;###autoload
(cl-defun launcher-snippets-tool (prefix &key (name "Snippets") icon)
  "Return a `launcher-tools' entry for the snippets in `launcher-snippets-file'.
`PREFIX ' alone lists every snippet and further input narrows the list.
RET fills the placeholders and copies the result, then runs
`launcher-after-copy-hook'.  NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit #'launcher-snippets-submit
        :candidates #'launcher-snippets--read))

(provide 'launcher-snippets)
;;; launcher-snippets.el ends here
