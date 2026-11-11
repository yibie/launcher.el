;;; launcher-system-tests.el --- Tests for launcher-system -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'launcher)
(require 'launcher-system)

(defun launcher-system-test--wait (process)
  "Wait until PROCESS has exited and its sentinel has run."
  (while (process-live-p process)
    (accept-process-output process 0.05))
  ;; The sentinel runs from the event loop after the exit.
  (accept-process-output nil 0.05))

(ert-deftest launcher-system-default-commands-are-well-formed ()
  (let ((names (mapcar (lambda (c) (plist-get c :name)) launcher-system-commands)))
    (should (equal names (delete-dups (copy-sequence names))))
    (dolist (command launcher-system-commands)
      (should (stringp (plist-get command :name)))
      (should (or (stringp (plist-get command :shell))
                  (functionp (plist-get command :function))))
      (should (memq (plist-get command :output) '(nil copy show))))
    (dolist (name '("Restart" "Shut Down" "Log Out" "Empty Trash"))
      (should (plist-get (seq-find (lambda (c) (equal name (plist-get c :name)))
                                   launcher-system-commands)
                         :confirm)))))

(ert-deftest launcher-system-tool-is-valid-and-lists-commands ()
  (let ((launcher-tools (list (launcher-system-tool ">")))
        (launcher-system-commands
         (list (list :name "Echo" :shell "echo hi" :subtitle "Say hi"))))
    (should (launcher--tools))
    (let ((candidate (car (launcher-system--candidates))))
      (should (equal "Echo" (plist-get candidate :title)))
      (should (equal "Say hi" (plist-get candidate :subtitle)))
      (should (functionp (launcher-system--submit candidate))))))

(ert-deftest launcher-system-define-adds-and-replaces ()
  (let ((launcher-system-commands nil))
    (launcher-system-define "A" :shell "true")
    (launcher-system-define "B" :shell "true")
    (launcher-system-define "A" :shell "false" :confirm t)
    (should (equal '("A" "B") (mapcar (lambda (c) (plist-get c :name))
                                      launcher-system-commands)))
    (should (equal "false" (plist-get (car launcher-system-commands) :shell)))
    (should-error (launcher-system-define "C" :confirm t))))

(ert-deftest launcher-system-copy-output-runs-hook ()
  (let* ((kill-ring nil) (hooked nil)
         (launcher-after-copy-hook (list (lambda () (setq hooked t))))
         (process (launcher-system-run
                   (list :name "Echo" :shell "printf '  1.2.3\\n'" :output 'copy))))
    (launcher-system-test--wait process)
    (should (equal "1.2.3" (car kill-ring)))
    (should hooked)))

(ert-deftest launcher-system-show-output-in-buffer ()
  (let* ((launcher-system-output-buffer " *launcher-system-test-output*")
         (display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t)))
         (process (launcher-system-run
                   (list :name "Echo" :shell "echo shown" :output 'show))))
    (unwind-protect
        (progn
          (launcher-system-test--wait process)
          (should (equal "shown\n" (with-current-buffer launcher-system-output-buffer
                                     (buffer-string)))))
      (kill-buffer launcher-system-output-buffer))))

(ert-deftest launcher-system-failure-shows-error-buffer ()
  (let* ((launcher-system-error-buffer " *launcher-system-test-errors*")
         (display-buffer-overriding-action '(display-buffer-no-window (allow-no-window . t)))
         (kill-ring nil)
         (process (launcher-system-run
                   (list :name "Broken" :shell "echo oops; exit 3" :output 'copy))))
    (unwind-protect
        (progn
          (launcher-system-test--wait process)
          (should-not kill-ring)
          (let ((text (with-current-buffer launcher-system-error-buffer (buffer-string))))
            (should (string-match-p "Broken failed (exit 3)" text))
            (should (string-match-p "\\$ echo oops; exit 3" text))
            (should (string-match-p "oops" text))))
      (kill-buffer launcher-system-error-buffer))))

(ert-deftest launcher-system-confirm-declined-runs-nothing ()
  (let ((ran nil))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) nil)))
      (should-not (launcher-system-run
                   (list :name "Danger" :confirm t :function (lambda () (setq ran t))))))
    (should-not ran)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (launcher-system-run
       (list :name "Danger" :confirm t :function (lambda () (setq ran t)))))
    (should ran)))

(ert-deftest launcher-system-caffeinate-toggles ()
  (let ((launcher-system-caffeinate-command '("sleep" "60"))
        (launcher-system--caffeinate nil))
    (unwind-protect
        (progn
          (launcher-system-toggle-caffeinate)
          (should (launcher-system-caffeinated-p))
          (let ((launcher-system-commands
                 (list (list :name "Toggle Caffeinate"
                             :function #'launcher-system-toggle-caffeinate))))
            (should (equal "Toggle Caffeinate (on)"
                           (plist-get (car (launcher-system--candidates)) :title))))
          (launcher-system-toggle-caffeinate)
          (should-not (launcher-system-caffeinated-p)))
      (when (process-live-p launcher-system--caffeinate)
        (delete-process launcher-system--caffeinate)))))

;;; launcher-system-tests.el ends here
