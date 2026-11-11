;;; init-launcher.el --- The launcher setup the README screenshots show -*- lexical-binding: t -*-

;;; Commentary:
;; An example, not part of the package: copy what you need into your
;; configuration.  It installs this fork with Elpaca; with use-package,
;; replace the `elpaca' forms (see the README).
;;
;; Prefixes: no prefix  apps, calculator and sites
;;           s  sites          g  Google            w  every search engine
;;           f  files          z  zoxide directories
;;           >  system commands
;;           c  clipboard      e  emoji and symbols
;;           d  dictionary     t  snippets
;;
;; Paths below assume sites.org and snippets.org from this directory are
;; copied to ~/notes/.

;;; Code:

(elpaca (launcher :host github :repo "yibie/launcher.el" :branch "main"))
;; For the `d' dictionary tool.
(elpaca osx-dictionary)

(keymap-global-set "C-c l" #'launcher)

(with-eval-after-load 'launcher
  ;; Scan `launcher-app-directories' instead of asking Spotlight, which
  ;; finds nothing when indexing is off.
  (setq launcher-app-discovery-method 'filesystem)

  (require 'launcher-web)
  (require 'launcher-snippets)
  (require 'launcher-files)
  (require 'launcher-system)
  (require 'launcher-clipboard)
  (require 'launcher-characters)
  (require 'launcher-osx-dictionary)

  (setq launcher-sites-file (expand-file-name "~/notes/sites.org")
        launcher-snippets-file (expand-file-name "~/notes/snippets.org")
        launcher-snippets-weekday-names ["星期日" "星期一" "星期二" "星期三"
                                         "星期四" "星期五" "星期六"])

  ;; `f' searches these directories; `z' opens directories in Dired.
  (setq launcher-files-directories '("~/Documents" "~/Downloads")
        launcher-zoxide-open-function #'launcher-files-open-in-emacs)

  (setq launcher-tools
        (list (launcher-sites-tool nil)
              (launcher-sites-tool "s")
              (launcher-search-tool "g" :name "Google"
                                    :url "https://www.google.com/search?q=%s")
              (launcher-web-search-tool "w")
              (launcher-files-tool "f")
              (launcher-zoxide-tool "z")
              (launcher-system-tool ">")
              (launcher-clipboard-tool "c")
              (launcher-characters-tool "e")
              '("d" :name "Dictionary"
                :query launcher-osx-dictionary-query :debounce 0.2
                :submit launcher-osx-dictionary-submit)
              (launcher-snippets-tool "t")))

  ;; Recently chosen results first, the rest in the order tools gave them.
  (setq launcher-sort-function #'launcher-sort-by-history)

  (when (require 'nerd-icons nil t)
    (setq launcher-builtin-icons
          `((calculator . ,(nerd-icons-faicon "nf-fa-calculator"))
            (search     . ,(nerd-icons-faicon "nf-fa-search"))
            (url        . ,(nerd-icons-faicon "nf-fa-link"))))))

;; The global popup: see launcher-popup.el and raycast/.
(autoload 'my/launcher-popup "launcher-popup" nil t)

(provide 'init-launcher)
;;; init-launcher.el ends here
