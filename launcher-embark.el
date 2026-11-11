;;; launcher-embark.el --- Embark targets for launcher results -*- lexical-binding: t; -*-

;;; Commentary:
;; Launcher's completion candidates are opaque keys of category
;; `launcher-result', so Embark cannot tell a clipboard entry from an
;; application.  `launcher-embark-transform' turns the result of a tool
;; registered in `launcher-embark-categories' into a target of that tool's
;; own category, whose value is the result's :data.  A tool then gives
;; Embark a keymap for its category in `embark-keymap-alist'.
;;
;; The transformer is installed once Embark loads.

;;; Code:

(require 'cl-lib)

(declare-function launcher--result-for-candidate "launcher-query")
(declare-function launcher--tool-submit "launcher")
(defvar embark-transformer-alist)

(defvar launcher-embark-categories nil
  "Alist (SUBMIT . CATEGORY) mapping tools to Embark categories.
SUBMIT is a tool's :submit function.  The results of that tool become
Embark targets of CATEGORY whose value is the result's :data string.")

(defun launcher-embark-register (submit category)
  "Make results of the tool whose :submit is SUBMIT Embark targets of CATEGORY."
  (setf (alist-get submit launcher-embark-categories) category))

(defun launcher-embark-transform (type target)
  "Turn TARGET of TYPE `launcher-result' into its tool's Embark target.
Return (CATEGORY . DATA) for a registered tool with string :data, and
\(TYPE . TARGET) otherwise."
  (let* ((result (launcher--result-for-candidate target))
         (tool (plist-get result :tool))
         (category (and tool (alist-get (launcher--tool-submit tool)
                                        launcher-embark-categories)))
         (data (plist-get (plist-get result :item) :data)))
    (if (and category (stringp data))
        (cons category data)
      (cons type target))))

(with-eval-after-load 'embark
  (setf (alist-get 'launcher-result embark-transformer-alist)
        #'launcher-embark-transform))

(provide 'launcher-embark)
;;; launcher-embark.el ends here
