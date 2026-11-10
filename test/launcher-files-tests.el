;;; launcher-files-tests.el --- Tests for launcher-files -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'launcher)
(require 'launcher-files)

(defmacro launcher-files-test--with-tree (&rest body)
  "Run BODY with `root' a temporary tree and the cache file inside it."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "launcher-files" t)))
          (launcher-files-directories (list (expand-file-name "docs" root)))
          (launcher-files-cache-file (expand-file-name "cache/files.txt" root))
          (launcher-files-ignore-globs '("build")))
     (unwind-protect
         (progn
           (dolist (file '("docs/notes/emacs-launcher.org" "docs/report 2026.pdf"
                           "docs/build/junk-launcher.o" "docs/.hidden-launcher"))
             (let ((path (expand-file-name file root)))
               (make-directory (file-name-directory path) t)
               (write-region "" nil path nil 'silent)))
           ,@body)
       (delete-directory root t))))

(ert-deftest launcher-files-refresh-lists-files-without-ignored ()
  (skip-unless (executable-find "rg"))
  (launcher-files-test--with-tree
    (launcher-files-refresh t)
    (let ((files (with-temp-buffer
                   (insert-file-contents launcher-files-cache-file)
                   (split-string (buffer-string) "\n" t))))
      (should (= 2 (length files)))
      (should (cl-every #'file-name-absolute-p files))
      (should-not (cl-some (lambda (f) (string-match-p "junk\\|hidden" f)) files)))))

(ert-deftest launcher-files-query-ranks-with-fzf ()
  (skip-unless (and (executable-find "rg") (executable-find "fzf")))
  (launcher-files-test--with-tree
    ;; No cache yet: the first query builds it before answering.
    (let ((results (launcher-files--query "emacs launcher")))
      ;; fzf matches fuzzily across the whole path; the best match comes first.
      (should (equal "emacs-launcher.org" (plist-get (car results) :title)))
      (should (string-suffix-p "docs/notes/" (plist-get (car results) :subtitle)))
      (should (file-exists-p (plist-get (car results) :data))))
    (should (equal "report 2026.pdf"
                   (plist-get (car (launcher-files--query "report 2026")) :title)))
    (should-not (launcher-files--query "e"))
    (should-not (launcher-files--query "zzzzqqq"))))

(ert-deftest launcher-files-stale-after-interval ()
  (launcher-files-test--with-tree
    (should (launcher-files--stale-p))
    (make-directory (file-name-directory launcher-files-cache-file) t)
    (write-region "" nil launcher-files-cache-file nil 'silent)
    (let ((launcher-files-refresh-interval 900))
      (should-not (launcher-files--stale-p)))
    (set-file-times launcher-files-cache-file (time-subtract nil 1000))
    (let ((launcher-files-refresh-interval 900))
      (should (launcher-files--stale-p)))))

(ert-deftest launcher-files-submit-opens-after-close ()
  (let* (opened (launcher-files-open-function (lambda (path) (setq opened path)))
         (action (launcher-files--submit '(:data "/tmp/x.pdf"))))
    (should (functionp action))
    (should-not opened)
    (should-not (funcall action))
    (should (equal "/tmp/x.pdf" opened))))

(ert-deftest launcher-zoxide-candidates-and-submit ()
  (let* (calls opened (launcher-files-open-function (lambda (path) (setq opened path))))
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/bin/zoxide"))
              ((symbol-function 'process-lines)
               (lambda (&rest args) (push args calls)
                 (list (expand-file-name "~/a") "/opt/b")))
              ((symbol-function 'call-process)
               (lambda (&rest args) (push args calls) 0)))
      (let ((candidates (launcher-zoxide--candidates)))
        (should (equal '("~/a" "/opt/b") (mapcar (lambda (c) (plist-get c :title)) candidates)))
        (funcall (launcher-zoxide--submit (cadr candidates)))
        (should (equal "/opt/b" opened))
        (should (equal '("zoxide" nil 0 nil "add" "--" "/opt/b") (car calls)))))))

(ert-deftest launcher-files-tools-validate ()
  (should (launcher--tool-from-entry (launcher-files-tool "f")))
  (should (launcher--tool-from-entry (launcher-zoxide-tool "z" :name "Dirs"))))

;;; launcher-files-tests.el ends here
