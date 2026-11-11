;;; launcher-files.el --- File and directory search for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Two tools for launcher that need no Spotlight index:
;;
;; `launcher-files-tool' searches a cached list of the files under
;; `launcher-files-directories'.  ripgrep writes the list to
;; `launcher-files-cache-file' in the background; fzf's non-interactive
;; filter ranks it per query, so a few hundred thousand files stay fast.
;;
;; `launcher-zoxide-tool' offers the directories zoxide has learned, most
;; frequently and recently used first.
;;
;; Files open with `launcher-files-open-function', directories with
;; `launcher-zoxide-open-function' (by default the same function).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function launcher-editing-frame "launcher")

(defcustom launcher-files-directories '("~/Documents" "~/Downloads")
  "Directories whose files `launcher-files-tool' searches.
Run `launcher-files-refresh' after changing this option."
  :type '(repeat directory)
  :group 'launcher)

(defcustom launcher-files-ignore-globs
  '("build" ".build" "*.noindex" "DerivedData" "node_modules" "__pycache__"
    ".venv" "eln-cache" ".cache")
  "Names left out of the file list, as ripgrep globs.
Each becomes `--glob !GLOB'; a name without a slash matches at any depth.
Files ignored by .gitignore and hidden files are always left out."
  :type '(repeat string)
  :group 'launcher)

(defcustom launcher-files-cache-file (locate-user-emacs-file "launcher-files.txt")
  "File holding the cached file list, one absolute path per line."
  :type 'file
  :group 'launcher)

(defcustom launcher-files-refresh-interval 900
  "Seconds after which a search starts rebuilding the file list.
The rebuild runs in the background; the search uses the old list meanwhile."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-files-limit 50
  "Most files a query shows."
  :type 'natnum
  :group 'launcher)

(defcustom launcher-files-open-function #'launcher-files-open-externally
  "Function called with the absolute path of a selected file or directory."
  :type '(choice (function-item launcher-files-open-externally)
                 (function-item launcher-files-open-in-emacs)
                 function)
  :group 'launcher)

(defcustom launcher-zoxide-open-function nil
  "Function called with the directory selected in `launcher-zoxide-tool'.
nil uses `launcher-files-open-function'."
  :type '(choice (const :tag "Same as files" nil)
                 (function-item launcher-files-open-externally)
                 (function-item launcher-files-open-in-emacs)
                 function)
  :group 'launcher)

(defun launcher-files-open-externally (path)
  "Open PATH with macOS `open': its default application, or Finder."
  (unless (zerop (call-process "open" nil nil nil path))
    (user-error "Cannot open %s" path)))

(defun launcher-files-open-in-emacs (path)
  "Visit PATH in Emacs: a file in its buffer, a directory in Dired.
When launcher runs in a minibuffer-only frame, PATH opens in another
frame, which takes the input focus."
  (launcher-editing-frame)
  (find-file path))

;;; File list

(defvar launcher-files--process nil "The running ripgrep rebuild, or nil.")

(defun launcher-files--command ()
  "Return the shell command that rebuilds `launcher-files-cache-file'."
  (let* ((cache (expand-file-name launcher-files-cache-file))
         (tmp (concat cache ".tmp")))
    (concat
     (mapconcat #'shell-quote-argument
                (append '("rg" "--files" "--no-messages")
                        (mapcan (lambda (glob) (list "--glob" (concat "!" glob)))
                                launcher-files-ignore-globs)
                        (mapcar #'expand-file-name launcher-files-directories))
                " ")
     " > " (shell-quote-argument tmp)
     ;; rg exits 1 when it finds nothing and 2 on unreadable entries; keep any list.
     "; test -s " (shell-quote-argument tmp)
     " && mv -f " (shell-quote-argument tmp) " " (shell-quote-argument cache))))

(defun launcher-files-refresh (&optional wait)
  "Rebuild the file list in the background; with WAIT, return when done."
  (interactive)
  (unless (executable-find "rg")
    (user-error "launcher-files needs ripgrep (rg) in PATH"))
  (make-directory (file-name-directory (expand-file-name launcher-files-cache-file)) t)
  (if wait
      (call-process shell-file-name nil nil nil shell-command-switch
                    (launcher-files--command))
    (unless (process-live-p launcher-files--process)
      (setq launcher-files--process
            (make-process :name "launcher-files" :noquery t
                          :command (list shell-file-name shell-command-switch
                                         (launcher-files--command))
                          :sentinel #'ignore)))))

(defun launcher-files--stale-p ()
  "Whether the cached file list is older than `launcher-files-refresh-interval'."
  (let ((attributes (file-attributes launcher-files-cache-file)))
    (or (null attributes)
        (> (float-time (time-since (file-attribute-modification-time attributes)))
           launcher-files-refresh-interval))))

(defun launcher-files--search (query)
  "Return up to `launcher-files-limit' cached paths matching QUERY, best first."
  (with-temp-buffer
    (call-process-shell-command
     (format "fzf --filter %s --scheme=path < %s | head -n %d"
             (shell-quote-argument query)
             (shell-quote-argument (expand-file-name launcher-files-cache-file))
             launcher-files-limit)
     nil t)
    (split-string (buffer-string) "\n" t)))

(defun launcher-files--query (query)
  "Return file candidates for QUERY, refreshing a stale list on the way."
  (unless (executable-find "fzf")
    (user-error "launcher-files needs fzf in PATH"))
  (cond
   ((not (file-exists-p launcher-files-cache-file)) (launcher-files-refresh t))
   ((launcher-files--stale-p) (launcher-files-refresh)))
  (let ((query (string-trim query)))
    (when (>= (length query) 2)
      (mapcar (lambda (path)
                (list :id path :title (file-name-nondirectory path)
                      :subtitle (abbreviate-file-name (file-name-directory path))
                      :data path))
              (launcher-files--search query)))))

(defun launcher-files--submit (candidate)
  "Open the path of CANDIDATE once launcher has closed."
  (let ((path (plist-get candidate :data)))
    (lambda () (funcall launcher-files-open-function path) nil)))

;;;###autoload
(cl-defun launcher-files-tool (prefix &key (name "Files") icon (debounce 0.1))
  "Return a `launcher-tools' entry searching files under PREFIX.
Queries of two or more characters are matched by fzf against the list
of files under `launcher-files-directories'.  RET opens the file with
`launcher-files-open-function'.  NAME, ICON and DEBOUNCE are as in
`launcher-tools'."
  (list prefix :name name :icon icon :debounce debounce
        :submit #'launcher-files--submit
        :query #'launcher-files--query))

;;; Directories from zoxide

(defun launcher-zoxide--candidates ()
  "Return zoxide's directories as candidates, best ranked first."
  (when (executable-find "zoxide")
    (mapcar (lambda (dir)
              (list :id dir :title (abbreviate-file-name dir) :data dir))
            (ignore-errors (process-lines "zoxide" "query" "--list")))))

(defun launcher-zoxide--submit (candidate)
  "Open CANDIDATE's directory once launcher has closed, and tell zoxide."
  (let ((dir (plist-get candidate :data)))
    (lambda ()
      (call-process "zoxide" nil 0 nil "add" "--" dir)
      (funcall (or launcher-zoxide-open-function launcher-files-open-function) dir)
      nil)))

;;;###autoload
(cl-defun launcher-zoxide-tool (prefix &key (name "Directories") icon)
  "Return a `launcher-tools' entry for zoxide's directories under PREFIX.
`PREFIX ' alone lists them, most used first; further input narrows the
list.  RET opens the directory with `launcher-zoxide-open-function' and
counts the visit in zoxide.  NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit #'launcher-zoxide--submit
        :candidates #'launcher-zoxide--candidates))

(provide 'launcher-files)
;;; launcher-files.el ends here
