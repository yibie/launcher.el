;;; launcher-web-tests.el --- Tests for launcher-web -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'launcher)
(require 'launcher-web)

(ert-deftest launcher-web-tool-offers-every-engine ()
  "`w 关键词' 每个引擎一条候选，URL 带编码后的关键词；空查询不出候选。"
  (let* ((launcher-web-search-engines '(("A" . "https://a.test/?q=%s")
                                         ("B" . "https://b.test/s/%s")))
         (tool (launcher-web-search-tool "w"))
         (query (plist-get (cdr tool) :query))
         (results (funcall query "emacs 中文")))
    (should (equal (car tool) "w"))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) results) '("A" "B")))
    (should (equal (plist-get (car results) :title) "A: emacs 中文"))
    (should (equal (plist-get (cadr results) :data)
                   (concat "https://b.test/s/" (url-hexify-string "emacs 中文"))))
    (should-not (funcall query "  "))))

(defun launcher-test--sites (content)
  "Return the candidates `launcher-sites--read' finds in CONTENT."
  (let ((file (make-temp-file "launcher-sites" nil ".org" content)))
    (unwind-protect (launcher-sites--read file)
      (delete-file file))))

(ert-deftest launcher-sites-read-org-links ()
  "site.org 里的带名链接、裸网址都成为候选；标题进副标题；重复网址只出一次。"
  (let ((sites (launcher-test--sites "\
#+title: 常用网址
* 工作
- [[https://github.com][GitHub]] 和 [[https://docs.google.com][Google Docs]]
- [[file:~/x.org][本地文件]]
* 社区
- https://lobste.rs
- [[https://github.com][GitHub 重复]]
")))
    (should (equal (mapcar (lambda (s) (plist-get s :title)) sites)
                   '("GitHub" "Google Docs" "lobste.rs")))
    (should (equal (plist-get (car sites) :subtitle) "工作 · https://github.com"))
    (should (equal (plist-get (nth 2 sites) :data) "https://lobste.rs"))
    (should-not (launcher-sites--read "/nonexistent/site.org"))))

(ert-deftest launcher-sites-tool-prefix-and-submit ()
  "不带前缀时混进默认搜索，带前缀时单独列出；回车交给 `browse-url'。"
  (let ((plain (launcher-sites-tool nil))
        (prefixed (launcher-sites-tool "s"))
        opened)
    (should-not (car plain))
    (should (equal (car prefixed) "s"))
    (should (eq (plist-get (cdr prefixed) :candidates) #'launcher-sites--read))
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
      (should-not (funcall (plist-get (cdr plain) :submit) '(:data "https://lobste.rs")))
      (should (equal opened "https://lobste.rs")))))

(ert-deftest launcher-web-tool-opens-selected-url ()
  "回车把所选引擎的 URL 交给 `browse-url'，返回 nil 让 launcher 关帧。"
  (let* ((submit (plist-get (cdr (launcher-web-search-tool "w")) :submit))
         opened)
    (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
      (should-not (funcall submit '(:id "A" :data "https://a.test/?q=x")))
      (should (equal opened "https://a.test/?q=x")))))

(ert-deftest launcher-web-tools-validate ()
  "两个构造函数的条目都能通过 launcher 的校验。"
  (should (launcher--tool-from-entry (launcher-web-search-tool "w")))
  (should (launcher--tool-from-entry (launcher-sites-tool "s" :name "Bookmarks"))))

;;; launcher-web-tests.el ends here
