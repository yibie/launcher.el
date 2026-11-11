;;; launcher-clipboard.el --- Clipboard history for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-clipboard-tool' lists the kill ring, newest first, with the
;; system clipboard folded in.  RET copies the entry again, moving it to
;; the front of the kill ring, and runs `launcher-after-copy-hook'.
;;
;; With Embark, entries are targets of category `launcher-clipboard' and
;; `launcher-clipboard-embark-map' offers:
;;
;;   e  edit the entry in a buffer; C-c C-c copies the result in its place
;;   d  delete the entry from the kill ring
;;   j  join the entries, in the order they were selected with
;;      `embark-select', and copy them as one text (`embark-act-all';
;;      without a selection, every listed entry)
;;
;; Its parent is `embark-general-map', so SPC selects an entry.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'launcher-embark)

(defvar launcher-after-copy-hook)
(defvar embark-general-map)
(defvar embark-keymap-alist)
(defvar embark-multitarget-actions)
(declare-function launcher-editing-frame "launcher")

(defcustom launcher-clipboard-title-width 80
  "Characters of an entry shown as its title."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-clipboard-join-separator "\n"
  "Text put between entries joined by `launcher-clipboard-join'."
  :type 'string
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

;;; Actions

(defun launcher-clipboard-copy (text &optional message)
  "Put TEXT at the front of the kill ring, once, and run the copy hook.
MESSAGE is echoed instead of the default."
  (setq kill-ring (delete text kill-ring))
  (kill-new text)
  (message "%s" (or message (format "Copied: %s" (launcher-clipboard--title text))))
  (run-hooks 'launcher-after-copy-hook))

(defun launcher-clipboard-delete (text)
  "Remove every copy of TEXT from the kill ring."
  (setq kill-ring (delete text kill-ring)
        kill-ring-yank-pointer kill-ring)
  (message "Deleted: %s" (launcher-clipboard--title text)))

(defun launcher-clipboard-join (texts)
  "Copy TEXTS joined by `launcher-clipboard-join-separator', in order.
TEXTS may also be a single string."
  (let ((texts (if (stringp texts) (list texts) texts)))
    (launcher-clipboard-copy
     (string-join texts launcher-clipboard-join-separator)
     (format "Copied %d entr%s joined" (length texts)
             (if (= 1 (length texts)) "y" "ies")))))

(defvar-local launcher-clipboard--original nil
  "The kill ring entry the current edit buffer started from.")

(defvar-keymap launcher-clipboard-edit-mode-map
  "C-c C-c" #'launcher-clipboard-edit-finish
  "C-c C-k" #'launcher-clipboard-edit-abort)

(define-minor-mode launcher-clipboard-edit-mode
  "Minor mode for editing a kill ring entry.
\\<launcher-clipboard-edit-mode-map>\\[launcher-clipboard-edit-finish] copies the text in place of the entry, \
\\[launcher-clipboard-edit-abort] discards the edit."
  :lighter " Clip-Edit"
  (setq header-line-format
        (and launcher-clipboard-edit-mode
             (substitute-command-keys
              "Edit clipboard entry: \\<launcher-clipboard-edit-mode-map>\
\\[launcher-clipboard-edit-finish] copy, \\[launcher-clipboard-edit-abort] cancel"))))

(defun launcher-clipboard-edit (text)
  "Edit the kill ring entry TEXT in a buffer.
\\<launcher-clipboard-edit-mode-map>\\[launcher-clipboard-edit-finish] \
replaces the entry with the edited text and copies it."
  (launcher-editing-frame)
  (let ((buffer (generate-new-buffer "*launcher-clipboard edit*")))
    (with-current-buffer buffer
      (insert text)
      (goto-char (point-min))
      (text-mode)
      (launcher-clipboard-edit-mode)
      (setq launcher-clipboard--original text)
      (set-buffer-modified-p nil))
    (pop-to-buffer buffer)))

(defun launcher-clipboard-edit-finish ()
  "Copy the edited text in place of the entry it started from."
  (interactive)
  (let ((text (buffer-substring-no-properties (point-min) (point-max))))
    (setq kill-ring (delete launcher-clipboard--original kill-ring))
    (quit-window t)
    (launcher-clipboard-copy text)))

(defun launcher-clipboard-edit-abort ()
  "Discard the edit and leave the kill ring unchanged."
  (interactive)
  (quit-window t)
  (message "Edit discarded"))

(defvar launcher-clipboard-embark-map (make-sparse-keymap)
  "Embark actions on `launcher-clipboard' targets.")
(define-key launcher-clipboard-embark-map (kbd "RET") #'launcher-clipboard-copy)
(define-key launcher-clipboard-embark-map "e" #'launcher-clipboard-edit)
(define-key launcher-clipboard-embark-map "d" #'launcher-clipboard-delete)
(define-key launcher-clipboard-embark-map "j" #'launcher-clipboard-join)

(with-eval-after-load 'embark
  (set-keymap-parent launcher-clipboard-embark-map embark-general-map)
  (setf (alist-get 'launcher-clipboard embark-keymap-alist)
        '(launcher-clipboard-embark-map))
  (add-to-list 'embark-multitarget-actions #'launcher-clipboard-join))

;;; Tool

(defun launcher-clipboard--submit (candidate)
  "Copy CANDIDATE's text once launcher has closed."
  (let ((text (plist-get candidate :data)))
    (lambda () (launcher-clipboard-copy text) nil)))

(launcher-embark-register #'launcher-clipboard--submit 'launcher-clipboard)

;;;###autoload
(cl-defun launcher-clipboard-tool (prefix &key (name "Clipboard") icon)
  "Return a `launcher-tools' entry for the kill ring under PREFIX.
`PREFIX ' alone lists every entry, newest first; further input narrows
the list.  RET copies the entry and runs `launcher-after-copy-hook'.
With Embark, `launcher-clipboard-embark-map' edits, deletes and joins
entries.  NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit #'launcher-clipboard--submit
        :candidates #'launcher-clipboard--candidates))

(provide 'launcher-clipboard)
;;; launcher-clipboard.el ends here
