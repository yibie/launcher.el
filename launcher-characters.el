;;; launcher-characters.el --- Emoji and symbols for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-characters-tool' finds emoji and other Unicode characters by
;; name.  Emoji and their names come from macOS's own emoji data (CoreEmoji,
;; see `launcher-characters-emoji-directory'), in every language of
;; `launcher-characters-emoji-languages', so `e heart' and `e 红色' both
;; work.  They are fully qualified (❤️ rather than ❤) and include families
;; and flags.  Other characters come from `ucs-names'.  Every word of the
;; query must occur in a name.  The exact name comes first, then emoji,
;; then shorter names.  RET copies the character and runs
;; `launcher-after-copy-hook'.
;;
;; Emoji titles use the face `launcher-characters-emoji', which names a
;; color emoji font: otherwise a symbol the default font also has (❤, ☀)
;; is drawn in that font, without color.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar launcher-after-copy-hook)

(defcustom launcher-characters-limit 60
  "Most characters a query shows."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-characters-emoji-directory
  "/System/Library/PrivateFrameworks/CoreEmoji.framework/Versions/A/Resources"
  "Directory with macOS's emoji names, LANG.lproj/AppleName.strings.
Without it, emoji are found only by their Unicode names in `ucs-names'."
  :type 'directory
  :group 'launcher)

(defcustom launcher-characters-emoji-languages '("en" "zh_CN")
  "Languages whose emoji names are searched; the first names the title.
Each is the name of a LANG.lproj directory in
`launcher-characters-emoji-directory'."
  :type '(repeat string)
  :group 'launcher)

(defface launcher-characters-emoji
  '((((type ns)) :family "Apple Color Emoji" :height 0.8)
    (t :family "Noto Color Emoji" :height 0.8))
  "Face of the emoji in `launcher-characters-tool' titles.
The reduced height keeps a line of emoji as tall as a line of text: a
color emoji font is taller than most text fonts, and a completion list
of taller lines may not fit a minibuffer sized in lines, for instance
in a popup frame, where Vertico's reversed list then scrolls out of view."
  :group 'launcher)

;;; Emoji data

(defvar launcher-characters--emoji nil
  "Emoji as (STRING NAMES UPCASED-NAMES), or t when none could be read.
NAMES holds one name per language of `launcher-characters-emoji-languages'
that has one.")

(defun launcher-characters--read-names (language)
  "Return the emoji names of LANGUAGE as an alist (EMOJI . NAME), or nil."
  (let ((file (expand-file-name (concat language ".lproj/AppleName.strings")
                                launcher-characters-emoji-directory)))
    (when (and (file-readable-p file) (executable-find "plutil"))
      (with-temp-buffer
        (when (zerop (call-process "plutil" nil t nil
                                   "-convert" "json" "-o" "-" file))
          (goto-char (point-min))
          (mapcar (lambda (pair) (cons (symbol-name (car pair)) (cdr pair)))
                  (json-parse-buffer :object-type 'alist)))))))

(defun launcher-characters--emoji ()
  "Return the emoji records, reading macOS's emoji names the first time."
  (when (null launcher-characters--emoji)
    (let ((names (make-hash-table :test #'equal)) (order nil))
      (dolist (language launcher-characters-emoji-languages)
        (dolist (pair (ignore-errors (launcher-characters--read-names language)))
          (unless (gethash (car pair) names) (push (car pair) order))
          (puthash (car pair) (append (gethash (car pair) names) (list (cdr pair)))
                   names)))
      (setq launcher-characters--emoji
            (or (mapcar (lambda (emoji)
                          (let ((list (gethash emoji names)))
                            (list emoji list (mapcar #'upcase list))))
                        (nreverse order))
                t))))
  (and (consp launcher-characters--emoji) launcher-characters--emoji))

(defun launcher-characters--base-char (string)
  "Return the one character STRING shows, ignoring variation selectors."
  (let ((chars (cl-remove-if (lambda (c) (<= #xFE00 c #xFE0F)) (append string nil))))
    (and (= 1 (length chars)) (car chars))))

;;; Search

(defun launcher-characters--search (query)
  "Return (STRING NAMES EMOJI-P) for characters matching every word of QUERY.
A word matches when it occurs in one of the names.  The exact name comes
first, then emoji, then shorter names.  An ASCII query shorter than two
characters matches nothing; one other character (such as 心) suffices."
  (let* ((words (split-string (upcase query) nil t))
         (whole (string-join words " "))
         (emoji-chars (make-hash-table))
         (hits nil))
    (when (and words (or (>= (length whole) 2) (string-match-p "[^[:ascii:]]" whole)))
      ;; An integer key keeps the sort in C: rank, then length.
      (dolist (emoji (launcher-characters--emoji))
        (when-let* ((char (launcher-characters--base-char (car emoji))))
          (puthash char t emoji-chars))
        (let ((upcased (nth 2 emoji)))
          (when (cl-every (lambda (word)
                            (cl-some (lambda (name) (string-search word name)) upcased))
                          words)
            (push (cons (+ (if (member whole upcased) 0 100000) (length (car upcased)))
                        (list (car emoji) (nth 1 emoji) t))
                  hits))))
      ;; Tens of thousands of names: match before allocating anything.
      (maphash (lambda (name char)
                 (when (and (cl-every (lambda (word) (string-search word name)) words)
                            (not (gethash char emoji-chars)))
                   (let ((emoji-p (eq (aref char-script-table char) 'emoji)))
                     (push (cons (+ (if (equal name whole) 0 100000)
                                    (if emoji-p 0 10000)
                                    (length name))
                                 (list (char-to-string char) (list (capitalize name)) emoji-p))
                           hits))))
               (ucs-names))
      (mapcar #'cdr (seq-take (sort hits #'car-less-than-car)
                              launcher-characters-limit)))))

(defun launcher-characters--code-points (string)
  "Return STRING's code points as \"U+XXXX\" words."
  (mapconcat (lambda (char) (format "U+%04X" char)) string " "))

(defun launcher-characters--query (query)
  "Return character candidates for QUERY."
  (mapcar (pcase-lambda (`(,string ,names ,emoji-p))
            (let ((code-points (launcher-characters--code-points string)))
              (list :id code-points
                    :title (concat (if emoji-p
                                       (propertize string 'face 'launcher-characters-emoji)
                                     string)
                                   "  " (car names))
                    :subtitle (string-join (append (cdr names) (list code-points)) "  ")
                    :data string)))
          (launcher-characters--search query)))

(defun launcher-characters--submit (candidate)
  "Copy CANDIDATE's character once launcher has closed."
  (let ((text (plist-get candidate :data)))
    (lambda ()
      (kill-new text)
      (message "Copied %s" text)
      (run-hooks 'launcher-after-copy-hook)
      nil)))

;;;###autoload
(cl-defun launcher-characters-tool (prefix &key (name "Emoji & Symbols") icon
                                           (debounce 0))
  "Return a `launcher-tools' entry finding characters by name under PREFIX.
`PREFIX heart' lists emoji and characters whose name contains HEART,
emoji first; emoji also match their names in the other languages of
`launcher-characters-emoji-languages'.  RET copies the character and
runs `launcher-after-copy-hook'.  NAME, ICON and DEBOUNCE are as in
`launcher-tools'; a query takes a few milliseconds, so none is needed."
  (list prefix :name name :icon icon :debounce debounce
        :submit #'launcher-characters--submit
        :query #'launcher-characters--query))

(provide 'launcher-characters)
;;; launcher-characters.el ends here
