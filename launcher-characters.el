;;; launcher-characters.el --- Emoji and symbols for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-characters-tool' finds Unicode characters by name, using the
;; names Emacs knows (`ucs-names').  Every word of the query must occur in
;; the name.  Emoji come first, then shorter names.  RET copies the
;; character and runs `launcher-after-copy-hook'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar launcher-after-copy-hook)

(defcustom launcher-characters-limit 60
  "Most characters a query shows."
  :type 'natnum
  :group 'launcher)

(defun launcher-characters--emoji-p (char)
  "Whether CHAR belongs to the emoji script."
  (eq (aref char-script-table char) 'emoji))

(defun launcher-characters--search (query)
  "Return (NAME . CHAR) pairs whose name contains every word of QUERY.
The exact name comes first, then emoji, then shorter names.  A query
shorter than two characters matches nothing."
  (let* ((words (split-string (upcase query) nil t))
         (whole (string-join words " "))
         (hits nil))
    (when (>= (length whole) 2)
      (maphash (lambda (name char)
                 (when (cl-every (lambda (word) (string-search word name)) words)
                   ;; An integer key keeps the sort in C: rank, then length.
                   (push (cons (+ (if (equal name whole) 0 100000)
                                  (if (launcher-characters--emoji-p char) 0 10000)
                                  (length name))
                               (cons name char))
                         hits)))
               (ucs-names))
      (mapcar #'cdr (seq-take (sort hits #'car-less-than-car)
                              launcher-characters-limit)))))

(defun launcher-characters--query (query)
  "Return character candidates for QUERY."
  (mapcar (lambda (hit)
            (let ((char (cdr hit)))
              (list :id (format "U+%04X" char)
                    :title (format "%c  %s" char (capitalize (car hit)))
                    :subtitle (format "U+%04X" char)
                    :data (char-to-string char))))
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
                                           (debounce 0.1))
  "Return a `launcher-tools' entry finding characters by name under PREFIX.
`PREFIX heart' lists characters whose Unicode name contains HEART, emoji
first.  RET copies the character and runs `launcher-after-copy-hook'.
NAME, ICON and DEBOUNCE are as in `launcher-tools'."
  (list prefix :name name :icon icon :debounce debounce
        :submit #'launcher-characters--submit
        :query #'launcher-characters--query))

(provide 'launcher-characters)
;;; launcher-characters.el ends here
