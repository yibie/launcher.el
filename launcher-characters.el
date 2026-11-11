;;; launcher-characters.el --- Emoji and symbols for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-characters-tool' finds emoji and other Unicode characters by
;; name.  Emoji come from Emacs's emoji.el (fully qualified, so ❤ is the
;; colored ❤️), other characters from `ucs-names'.  Every word of the query
;; must occur in the name.  The exact name comes first, then emoji, then
;; shorter names.  RET copies the character and runs
;; `launcher-after-copy-hook'.
;;
;; Emoji titles use the face `launcher-characters-emoji', which names a
;; color emoji font: otherwise a symbol the default font also has (❤, ☀)
;; is drawn in that font, without color.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar launcher-after-copy-hook)
(defvar emoji--all-bases)
(declare-function emoji--init "emoji")

(defcustom launcher-characters-limit 60
  "Most characters a query shows."
  :type 'natnum
  :group 'launcher)

(defface launcher-characters-emoji
  '((((type ns)) :family "Apple Color Emoji")
    (t :family "Noto Color Emoji"))
  "Face of the emoji in `launcher-characters-tool' titles."
  :group 'launcher)

(defvar launcher-characters--emoji nil
  "Emoji as (UPCASED-NAME . STRING), or t when emoji.el offers none.")

(defun launcher-characters--emoji ()
  "Return emoji.el's emoji as (UPCASED-NAME . STRING) pairs, built once."
  (when (null launcher-characters--emoji)
    (setq launcher-characters--emoji
          (or (ignore-errors
                (require 'emoji)
                (emoji--init)
                (let (pairs)
                  (maphash (lambda (name string)
                             (push (cons (upcase name) string) pairs))
                           emoji--all-bases)
                  pairs))
              t)))
  (and (consp launcher-characters--emoji) launcher-characters--emoji))

(defun launcher-characters--base-char (string)
  "Return the one character STRING shows, ignoring variation selectors."
  (let ((chars (cl-remove-if (lambda (c) (<= #xFE00 c #xFE0F)) (append string nil))))
    (and (= 1 (length chars)) (car chars))))

(defun launcher-characters--search (query)
  "Return (NAME STRING EMOJI-P) for names containing every word of QUERY.
The exact name comes first, then emoji, then shorter names.  A query
shorter than two characters matches nothing."
  (let* ((words (split-string (upcase query) nil t))
         (whole (string-join words " "))
         (emoji-chars (make-hash-table))
         (hits nil))
    (when (>= (length whole) 2)
      (cl-flet ((add (name string emoji-p)
                  (when (cl-every (lambda (word) (string-search word name)) words)
                    ;; An integer key keeps the sort in C: rank, then length.
                    (push (cons (+ (if (equal name whole) 0 100000)
                                   (if emoji-p 0 10000)
                                   (length name))
                                (list name string emoji-p))
                          hits))))
        (dolist (pair (launcher-characters--emoji))
          (when-let* ((char (launcher-characters--base-char (cdr pair))))
            (puthash char t emoji-chars))
          (add (car pair) (cdr pair) t))
        (maphash (lambda (name char)
                   (unless (gethash char emoji-chars)
                     (add name (char-to-string char) nil)))
                 (ucs-names)))
      (mapcar #'cdr (seq-take (sort hits #'car-less-than-car)
                              launcher-characters-limit)))))

(defun launcher-characters--code-points (string)
  "Return STRING's code points as \"U+XXXX\" words."
  (mapconcat (lambda (char) (format "U+%04X" char)) string " "))

(defun launcher-characters--query (query)
  "Return character candidates for QUERY."
  (mapcar (pcase-lambda (`(,name ,string ,emoji-p))
            (let ((code-points (launcher-characters--code-points string)))
              (list :id code-points
                    :title (concat (if emoji-p
                                       (propertize string 'face 'launcher-characters-emoji)
                                     string)
                                   "  " (capitalize name))
                    :subtitle code-points
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
emoji first.  RET copies the character and runs
`launcher-after-copy-hook'.  NAME, ICON and DEBOUNCE are as in
`launcher-tools'; a query takes a few milliseconds, so none is needed."
  (list prefix :name name :icon icon :debounce debounce
        :submit #'launcher-characters--submit
        :query #'launcher-characters--query))

(provide 'launcher-characters)
;;; launcher-characters.el ends here
