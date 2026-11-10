;;; launcher-web.el --- Bookmarks and multi-engine web search for launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Two web tools for launcher: `launcher-sites-tool' offers the links of
;; an Org file (bookmarks), `launcher-web-search-tool' lists one search per
;; engine in `launcher-web-search-engines' for the query.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)

(defcustom launcher-sites-file (locate-user-emacs-file "sites.org")
  "Org file listing the websites `launcher-sites-tool' offers.
Every http(s) link in it is a site: [[URL][NAME]] is named NAME, a bare URL
is named by its host.  Headings only group the links; the one above a link
shows in its subtitle.  The file is read each time launcher opens."
  :type 'file
  :group 'launcher)

(defconst launcher-sites--link-re
  "\\[\\[\\(https?://[^]]+\\)\\]\\(?:\\[\\([^]]+\\)\\]\\)?\\]\\|\\(https?://[^][ \t\n<>\"]+\\)"
  "Match an org bracket link to http(s) (URL 1, NAME 2), or a bare URL (3).")

(defun launcher-sites--read (&optional file)
  "Return the sites in FILE (`launcher-sites-file') as launcher candidates.
A URL listed twice is offered once, where it first appears.  Nil when the
file does not exist."
  (let ((file (or file launcher-sites-file)) heading sites seen)
    (when (file-readable-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (pos-bol) (pos-eol))) (start 0))
            (if (string-match "\\`\\*+[ \t]+\\(.*?\\)[ \t]*\\'" line)
                (setq heading (match-string 1 line))
              (while (string-match launcher-sites--link-re line start)
                (setq start (match-end 0))
                (let* ((url (or (match-string 1 line) (match-string 3 line)))
                       (name (or (match-string 2 line)
                                 (url-host (url-generic-parse-url url))
                                 url)))
                  (unless (member url seen)
                    (push url seen)
                    (push (list :id url :title name
                                :subtitle (if heading (format "%s · %s" heading url) url)
                                :data url)
                          sites))))))
          (forward-line 1))))
    (nreverse sites)))

;;;###autoload
(cl-defun launcher-sites-tool (prefix &key (name "Sites") (icon '(:builtin url)))
  "Return a `launcher-tools' entry for the sites in `launcher-sites-file'.
Without PREFIX sites match by name among the applications, as Alfred's
bookmarks do.  With PREFIX, `PREFIX ' alone lists every site and further
input narrows the list.  RET opens the site in the browser.
NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit (lambda (candidate) (browse-url (plist-get candidate :data)) nil)
        :candidates #'launcher-sites--read))

(defcustom launcher-web-search-engines
  '(("Google"      . "https://www.google.com/search?q=%s")
    ("Bing"        . "https://www.bing.com/search?q=%s")
    ("DuckDuckGo"  . "https://duckduckgo.com/?q=%s")
    ("百度"        . "https://www.baidu.com/s?wd=%s")
    ("GitHub"      . "https://github.com/search?q=%s")
    ("YouTube"     . "https://www.youtube.com/results?search_query=%s")
    ("Bilibili"    . "https://search.bilibili.com/all?keyword=%s")
    ("Product Hunt" . "https://www.producthunt.com/search?q=%s")
    ("X (Twitter)" . "https://x.com/search?q=%s")
    ("Reddit"      . "https://www.reddit.com/search/?q=%s")
    ("Wikipedia"   . "https://en.wikipedia.org/w/index.php?search=%s")
    ("Emacs China" . "https://emacs-china.org/search?q=%s"))
  "Search engines offered by `launcher-web-search-tool', as (NAME . URL).
URL is a format string; %s receives the URL-encoded query."
  :type '(alist :key-type string :value-type string)
  :group 'launcher)

;;;###autoload
(cl-defun launcher-web-search-tool (prefix &key (name "Web search") (icon '(:builtin search)))
  "Return a `launcher-tools' entry searching the web under PREFIX.
`PREFIX query' lists one candidate per engine in
`launcher-web-search-engines'; RET opens the selected one.  Engine names
are the candidate ids, so launcher's selection history puts the engines
used most recently first.  NAME and ICON are as in `launcher-tools'."
  (list prefix :name name :icon icon
        :submit (lambda (candidate) (browse-url (plist-get candidate :data)) nil)
        :query (lambda (query)
                 (unless (string-blank-p query)
                   (mapcar (pcase-lambda (`(,name . ,url))
                             (list :id name :title (format "%s: %s" name query)
                                   :subtitle "Search the web"
                                   :data (format url (url-hexify-string query))))
                           launcher-web-search-engines)))))

(provide 'launcher-web)
;;; launcher-web.el ends here
