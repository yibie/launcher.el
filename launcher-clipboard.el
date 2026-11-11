;;; launcher-clipboard.el --- Clipboard history for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-clipboard-tool' lists the kill ring, newest first, with the
;; system clipboard folded in.  RET copies the entry again, moving it to
;; the front of the kill ring, and runs `launcher-after-copy-hook'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar launcher-after-copy-hook)

(defcustom launcher-clipboard-title-width 80
  "Characters of an entry shown as its title."
  :type 'natnum
  :group 'launcher)

(defun launcher-clipboard--title (text)
  "Return TEXT on one line, shortened to `launcher-clipboard-title-width'."
  (truncate-string-to-width
   (replace-regexp-in-string "[ \t]*\n[ \t]*" " ⏎ " (string-trim text))
   launcher-clipboard-title-width nil nil "…"))

(defun launcher-clipboard--candidates ()
  "Return the distinct kill ring entries as candidates, newest first."
  ;; Pulls text copied in other applications into the kill ring first.
  (ignore-errors (current-kill 0 t))
  (mapcar (lambda (text)
            (let ((lines (1+ (cl-count ?\n (string-trim-right text)))))
              (list :id (md5 text) :title (launcher-clipboard--title text)
                    :subtitle (format "%d line%s, %d chars"
                                      lines (if (= lines 1) "" "s") (length text))
                    :data text)))
          (delete-dups
           (cl-remove-if #'string-blank-p
                         (mapcar #'substring-no-properties kill-ring)))))

(defun launcher-clipboard--submit (candidate)
  "Copy CANDIDATE's text once launcher has closed."
  (let ((text (plist-get candidate :data)))
    (lambda ()
      (setq kill-ring (delete text kill-ring))
      (kill-new text)
      (message "Copied: %s" (launcher-clipboard--title text))
      (run-hooks 'launcher-after-copy-hook)
      nil)))

;;;###autoload
(cl-defun launcher-clipboard-tool (prefix &key (name "Clipboard") icon)
  "Return a `launcher-tools' entry for the kill ring under PREFIX.
`PREFIX ' alone lists every entry, newest first; further input narrows
the list.  RET copies the entry and runs `launcher-after-copy-hook'.
NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit #'launcher-clipboard--submit
        :candidates #'launcher-clipboard--candidates))

(provide 'launcher-clipboard)
;;; launcher-clipboard.el ends here
