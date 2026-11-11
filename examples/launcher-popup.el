;;; launcher-popup.el --- Summon launcher in a popup frame from anywhere -*- lexical-binding: t -*-

;;; Commentary:
;; An example, not part of the package: copy it into your configuration.
;;
;; A global hotkey (a Raycast script command, Karabiner, skhd...) runs
;;
;;   emacsclient -n -e '(my/launcher-popup)'
;;
;; which opens a small minibuffer-only frame in the middle of the screen,
;; runs `launcher' in it and deletes it afterwards.  Cancelling gives the
;; focus back to the application that was in front; so does an action
;; that only copied something (the calculator, a snippet, an emoji, a
;; clipboard entry), so you can paste it right away.  Other actions keep
;; the focus where they sent it: an application, a browser, a buffer.
;;
;; The idea comes from https://emacs-china.org/t/emacs-alfred/32124.
;; See raycast/summon-emacs-launcher.sh for the script command.

;;; Code:

(require 'cl-lib)
(require 'launcher)

(defvar vertico-count)

(defun my/popup--frontmost-app ()
  "Return the bundle id of the frontmost macOS application, or nil."
  (ignore-errors
    (let* ((asn (car (process-lines "lsappinfo" "front")))
           (info (process-lines "lsappinfo" "info" "-only" "bundleid" asn)))
      (cl-some (lambda (line)
                 (and (string-match "bundleID=\"\\([^\"]+\\)\"" line)
                      (match-string 1 line)))
               info))))

(defun my/popup--return-focus (frame)
  "Reactivate the application popup FRAME was summoned from."
  (when-let* ((origin (frame-parameter frame 'my-popup-origin)))
    (call-process "open" nil 0 nil "-b" origin)))

;; Whichever way the frame closes, the same function gives the focus back.
(add-hook 'delete-frame-functions #'my/popup--return-focus)

(defvar my/popup-return-focus nil
  "Non-nil gives the focus back after a successful action too.
Set by `launcher-after-copy-hook' while the popup runs.")

(defun my/popup--copied ()
  "Note that the action only copied its result."
  (setq my/popup-return-focus t))

(defun my/launcher-popup ()
  "Run `launcher' in a temporary minibuffer-only popup frame."
  (interactive)
  (let* ((origin (my/popup--frontmost-app))
         ;; Under a daemon there may be no graphical frame at all.
         (frame (make-frame `((window-system . ns)
                              (name . "launcher")
                              (minibuffer . only)
                              (my-popup-origin . ,origin)
                              (width . 80)
                              (height . ,(+ 2 (if (boundp 'vertico-count)
                                                  vertico-count 10))))))
         (my/popup-return-focus nil)
         (launcher-after-copy-hook (cons #'my/popup--copied
                                         launcher-after-copy-hook)))
    (pcase-let ((`(,x ,y ,width ,height) (frame-monitor-workarea frame)))
      (set-frame-position frame
                          (+ x (/ (- width (frame-outer-width frame)) 2))
                          (+ y (/ height 5))))
    (select-frame-set-input-focus frame)
    (unwind-protect
        (progn
          (call-interactively #'launcher)
          (unless my/popup-return-focus
            (set-frame-parameter frame 'my-popup-origin nil)))
      (when (frame-live-p frame)
        (delete-frame frame)))))

(provide 'launcher-popup)
;;; launcher-popup.el ends here
