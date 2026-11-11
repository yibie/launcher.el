;;; launcher-clipboard-tests.el --- Tests for clipboard and characters -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'launcher)
(require 'launcher-clipboard)
(require 'launcher-characters)

(ert-deftest launcher-clipboard-lists-distinct-entries ()
  (let ((kill-ring (list "new" "  " "line one\nline two" "new"))
        (interprogram-paste-function #'ignore)
        (launcher-tools (list (launcher-clipboard-tool "c"))))
    (should (launcher--tools))
    (let ((candidates (launcher-clipboard--candidates)))
      (should (equal '("new" "line one ⏎ line two")
                     (mapcar (lambda (c) (plist-get c :title)) candidates)))
      (should (equal "2 lines, 17 chars" (plist-get (nth 1 candidates) :subtitle)))
      (should (equal "line one\nline two" (plist-get (nth 1 candidates) :data))))))

(ert-deftest launcher-clipboard-includes-system-clipboard ()
  (let ((kill-ring (list "old")) (kill-ring-yank-pointer nil)
        (interprogram-paste-function (lambda () "from another app")))
    (should (equal "from another app"
                   (plist-get (car (launcher-clipboard--candidates)) :data)))))

(ert-deftest launcher-clipboard-title-is-truncated ()
  (let ((launcher-clipboard-title-width 10))
    (should (<= (string-width (launcher-clipboard--title (make-string 50 ?x))) 10))))

(ert-deftest launcher-clipboard-submit-moves-entry-to-front ()
  (let* ((kill-ring (list "a" "b" "c")) (hooked nil)
         (interprogram-cut-function nil) (interprogram-paste-function #'ignore)
         (launcher-after-copy-hook (list (lambda () (setq hooked t))))
         (action (launcher-clipboard--submit (list :data "c"))))
    (should (functionp action))
    (should-not hooked)
    (should-not (funcall action))
    (should (equal '("c" "a" "b") kill-ring))
    (should hooked)))

(ert-deftest launcher-characters-finds-emoji-first ()
  (let ((launcher-tools (list (launcher-characters-tool "e")))
        (results (launcher-characters--query "grinning face")))
    (should (launcher--tools))
    (should (equal "😀" (plist-get (car results) :data)))
    (should (equal "U+1F600" (plist-get (car results) :subtitle)))
    (should (string-suffix-p "Grinning Face" (plist-get (car results) :title)))
    (should (eq 'launcher-characters-emoji
                (get-text-property 0 'face (plist-get (car results) :title))))
    (should-not (launcher-characters--query "   "))
    (should-not (launcher-characters--query "zzzz qqqq"))))

(ert-deftest launcher-characters-emoji-are-fully-qualified ()
  (skip-unless (launcher-characters--emoji))
  (let* ((results (launcher-characters--query "red heart"))
         (data (mapcar (lambda (c) (plist-get c :data)) results)))
    (should (equal "❤️" (car data)))
    (should (equal "U+2764 U+FE0F" (plist-get (car results) :subtitle)))
    ;; The bare symbol is not offered a second time.
    (should-not (member "❤" (mapcar (lambda (c) (plist-get c :data))
                                    (launcher-characters--query "heavy black heart"))))))

(ert-deftest launcher-characters-symbols-and-limit ()
  (let ((arrow (seq-find (lambda (c) (equal "→" (plist-get c :data)))
                         (launcher-characters--query "rightwards arrow"))))
    (should arrow)
    (should-not (get-text-property 0 'face (plist-get arrow :title))))
  (let ((launcher-characters-limit 5))
    (should (= 5 (length (launcher-characters--query "arrow"))))))

(ert-deftest launcher-characters-submit-copies-and-runs-hook ()
  (let* ((kill-ring nil) (hooked nil) (interprogram-cut-function nil)
         (launcher-after-copy-hook (list (lambda () (setq hooked t)))))
    (funcall (launcher-characters--submit (list :data "★")))
    (should (equal "★" (car kill-ring)))
    (should hooked)))

;;; launcher-clipboard-tests.el ends here
