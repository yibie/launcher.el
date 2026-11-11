;;; launcher-system.el --- System commands for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; `launcher-system-tool' lists macOS system commands: appearance, power,
;; Trash, system information and System Settings panels.  Each command is a
;; plist in `launcher-system-commands':
;;
;;   :name     Title shown in launcher (required, unique).
;;   :shell    Shell command string run asynchronously, or
;;   :function Function called with no argument.
;;   :confirm  Non-nil asks before running.
;;   :output   nil reports success only; `copy' puts the trimmed output on
;;             the kill ring and runs `launcher-after-copy-hook'; `show'
;;             displays it in a buffer.
;;   :subtitle Short description.
;;
;; Commands run after launcher has closed.  A failing shell command shows
;; its command line and output in `launcher-system-error-buffer'.
;; `launcher-system-define' adds or replaces a command.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar launcher-after-copy-hook)

(defun launcher-system--osascript (script)
  "Return a shell command running AppleScript SCRIPT."
  (concat "osascript -e " (shell-quote-argument script)))

(defun launcher-system--settings (title pane)
  "Return a command opening System Settings PANE under TITLE."
  (list :name (concat "Settings: " title)
        :shell (concat "open " (shell-quote-argument
                                (concat "x-apple.systempreferences:" pane)))
        :subtitle "System Settings"))

(defcustom launcher-system-commands
  (append
   (list
    (list :name "Toggle Dark Mode"
          :shell (launcher-system--osascript
                  "tell application \"System Events\" to tell appearance preferences to set dark mode to not dark mode")
          :subtitle "Switch between light and dark appearance")
    (list :name "Turn Off Display" :shell "pmset displaysleepnow"
          :subtitle "Put the display to sleep now")
    (list :name "Toggle Hidden Files in Finder"
          :shell (concat "if [ \"$(defaults read com.apple.finder AppleShowAllFiles 2>/dev/null)\" = 1 ]; "
                         "then defaults write com.apple.finder AppleShowAllFiles -bool false; "
                         "else defaults write com.apple.finder AppleShowAllFiles -bool true; fi; "
                         "killall Finder")
          :subtitle "Show or hide dotfiles; restarts Finder")
    (list :name "Lock Screen"
          :shell (launcher-system--osascript
                  "tell application \"System Events\" to keystroke \"q\" using {control down, command down}")
          :subtitle "Needs Accessibility permission for Emacs")
    (list :name "Sleep" :shell "pmset sleepnow" :subtitle "Put the Mac to sleep")
    (list :name "Restart" :confirm t
          :shell (launcher-system--osascript "tell application \"System Events\" to restart")
          :subtitle "Applications may ask to save first")
    (list :name "Shut Down" :confirm t
          :shell (launcher-system--osascript "tell application \"System Events\" to shut down")
          :subtitle "Applications may ask to save first")
    (list :name "Log Out" :confirm t
          :shell (launcher-system--osascript "tell application \"System Events\" to log out")
          :subtitle "Applications may ask to save first")
    (list :name "Toggle Caffeinate" :function #'launcher-system-toggle-caffeinate
          :subtitle "Keep the Mac and display awake")
    (list :name "Empty Trash" :confirm t
          :shell (launcher-system--osascript "tell application \"Finder\" to empty trash")
          :subtitle "Delete the items in the Trash permanently")
    (list :name "macOS Version" :shell "sw_vers" :output 'copy
          :subtitle "Copy the product name, version and build")
    (list :name "Hardware Overview" :shell "system_profiler SPHardwareDataType"
          :output 'show :subtitle "Model, chip, memory, serial number")
    (list :name "External IP Address" :shell "curl -fsS --max-time 10 https://api.ipify.org"
          :output 'copy :subtitle "Asks api.ipify.org; copies the address"))
   (mapcar (lambda (pane) (apply #'launcher-system--settings pane))
           '(("General" "com.apple.systempreferences.GeneralSettings")
             ("Wi-Fi" "com.apple.wifi-settings-extension")
             ("Bluetooth" "com.apple.BluetoothSettings")
             ("Network" "com.apple.Network-Settings.extension")
             ("Displays" "com.apple.Displays-Settings.extension")
             ("Sound" "com.apple.Sound-Settings.extension")
             ("Appearance" "com.apple.Appearance-Settings.extension")
             ("Desktop & Dock" "com.apple.Desktop-Settings.extension")
             ("Control Center" "com.apple.ControlCenter-Settings.extension")
             ("Notifications" "com.apple.Notifications-Settings.extension")
             ("Keyboard" "com.apple.Keyboard-Settings.extension")
             ("Trackpad" "com.apple.Trackpad-Settings.extension")
             ("Battery" "com.apple.Battery-Settings.extension")
             ("Lock Screen" "com.apple.Lock-Screen-Settings.extension")
             ("Privacy & Security" "com.apple.settings.PrivacySecurity.extension")
             ("Users & Groups" "com.apple.Users-Groups-Settings.extension")
             ("Software Update" "com.apple.Software-Update-Settings.extension"))))
  "System commands offered by `launcher-system-tool'.
Each element is a plist; see the commentary of launcher-system.el for
its keys.  `launcher-system-define' adds or replaces one by name."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'launcher)

(defcustom launcher-system-caffeinate-command '("caffeinate" "-di")
  "Program and arguments `launcher-system-toggle-caffeinate' starts."
  :type '(repeat string)
  :group 'launcher)

(defcustom launcher-system-output-buffer "*launcher-system*"
  "Buffer showing the output of commands whose :output is `show'."
  :type 'string
  :group 'launcher)

(defcustom launcher-system-error-buffer "*launcher-system errors*"
  "Buffer showing the command line and output of a failed command."
  :type 'string
  :group 'launcher)

(defun launcher-system-define (name &rest props)
  "Add the command NAME with PROPS to `launcher-system-commands'.
PROPS are the keys described in launcher-system.el's commentary, without
:name.  A command already named NAME is replaced in place."
  (declare (indent 1))
  (unless (or (plist-get props :shell) (plist-get props :function))
    (error "launcher-system-define: %s needs :shell or :function" name))
  (let ((command (cons :name (cons name props)))
        (found nil))
    (setq launcher-system-commands
          (mapcar (lambda (old)
                    (if (equal (plist-get old :name) name)
                        (progn (setq found t) command)
                      old))
                  launcher-system-commands))
    (unless found
      (setq launcher-system-commands
            (append launcher-system-commands (list command))))
    command))

;;; Caffeinate

(defvar launcher-system--caffeinate nil "The running caffeinate process, or nil.")

(defun launcher-system-caffeinated-p ()
  "Whether `launcher-system-toggle-caffeinate' is keeping the Mac awake."
  (process-live-p launcher-system--caffeinate))

(defun launcher-system-toggle-caffeinate ()
  "Start `launcher-system-caffeinate-command', or stop it if it runs."
  (interactive)
  (if (launcher-system-caffeinated-p)
      (progn (delete-process launcher-system--caffeinate)
             (setq launcher-system--caffeinate nil)
             (message "Caffeinate off"))
    (setq launcher-system--caffeinate
          (make-process :name "launcher-caffeinate" :noquery t
                        :command launcher-system-caffeinate-command
                        :sentinel #'ignore))
    (message "Caffeinate on")))

;;; Running

(defun launcher-system--report-error (command output status)
  "Show COMMAND's shell line, exit STATUS and OUTPUT in the error buffer."
  (with-current-buffer (get-buffer-create launcher-system-error-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s failed (exit %s)\n\n$ %s\n\n%s"
                      (plist-get command :name) status
                      (plist-get command :shell) output))
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer))))

(defun launcher-system--finish (command output status)
  "Handle COMMAND having exited with STATUS and printed OUTPUT."
  (let ((name (plist-get command :name)))
    (if (not (eql status 0))
        (launcher-system--report-error command output status)
      (pcase (plist-get command :output)
        ('copy
         (let ((text (string-trim output)))
           (kill-new text)
           (message "%s: %s (copied)" name text)
           (run-hooks 'launcher-after-copy-hook)))
        ('show
         (with-current-buffer (get-buffer-create launcher-system-output-buffer)
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert output)
             (goto-char (point-min))
             (special-mode))
           (display-buffer (current-buffer))))
        (_ (message "%s: done" name))))))

(defun launcher-system-run (command)
  "Run COMMAND, a plist of `launcher-system-commands'.
Ask first when it has :confirm.  A :shell command runs asynchronously;
return its process, or nil when nothing was started."
  (let ((name (plist-get command :name)))
    (when (or (not (plist-get command :confirm))
              (yes-or-no-p (format "%s? " name)))
      (if-let* ((function (plist-get command :function)))
          (progn (funcall function) nil)
        (let ((buffer (generate-new-buffer " *launcher-system-process*")))
          (make-process
           :name "launcher-system" :buffer buffer :noquery t
           :command (list shell-file-name shell-command-switch
                          (plist-get command :shell))
           :sentinel
           (lambda (process _event)
             (unless (process-live-p process)
               (let ((output (with-current-buffer buffer (buffer-string))))
                 (kill-buffer buffer)
                 (launcher-system--finish command output
                                          (process-exit-status process)))))))))))

;;; Tool

(defun launcher-system--candidates ()
  "Return `launcher-system-commands' as launcher candidates."
  (mapcar (lambda (command)
            (let ((name (plist-get command :name)))
              (list :id name
                    :title (if (eq (plist-get command :function)
                                   #'launcher-system-toggle-caffeinate)
                               (format "%s (%s)" name
                                       (if (launcher-system-caffeinated-p) "on" "off"))
                             name)
                    :subtitle (plist-get command :subtitle)
                    :data command)))
          launcher-system-commands))

(defun launcher-system--submit (candidate)
  "Run CANDIDATE's command once launcher has closed."
  (let ((command (plist-get candidate :data)))
    (lambda () (launcher-system-run command) nil)))

;;;###autoload
(cl-defun launcher-system-tool (prefix &key (name "System") icon)
  "Return a `launcher-tools' entry for `launcher-system-commands' under PREFIX.
`PREFIX ' alone lists every command and further input narrows the list.
RET runs the command after launcher closes, asking first for those with
:confirm.  NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit #'launcher-system--submit
        :candidates #'launcher-system--candidates))

(provide 'launcher-system)
;;; launcher-system.el ends here
