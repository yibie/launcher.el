;;; launcher-snippets-tests.el --- Tests for launcher-snippets -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'launcher)
(require 'launcher-snippets)

(ert-deftest launcher-snippets-read-org-headings ()
  "每个有正文的标题是一个模板；首尾空行去掉；只做分组的空标题跳过。"
  (let* ((file (make-temp-file "launcher-snippets" nil ".org" "\
#+title: 模板
* 邮件
** 回复

{input:称呼}你好，

  正文
** 空的
* 引用
> {clipboard}
"))
         (snippets (unwind-protect (launcher-snippets--read file)
                     (delete-file file))))
    (should (equal (mapcar (lambda (s) (plist-get s :title)) snippets) '("回复" "引用")))
    (should (equal (plist-get (car snippets) :data) "{input:称呼}你好，\n\n  正文"))
    (should (equal (plist-get (car snippets) :subtitle) "{input:称呼}你好，"))
    (should-not (launcher-snippets--read "/nonexistent/snippets.org"))))

(ert-deftest launcher-snippet-expand-placeholders ()
  "{clipboard}/{date}/{time}/{input:X} 都会填；同一个 input 只问一次；其他花括号不动。"
  (let ((asked nil))
    (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) "https://a.test"))
              ((symbol-function 'format-time-string)
               (lambda (fmt &rest _) (if (equal fmt "%Y-%m-%d") "2026-10-23" "14:05"))))
      (should (equal (launcher-snippets-expand
                      "{input:名字}：{clipboard} {date} {time} {input:名字} {other} \\1"
                      (lambda (prompt &optional _initial) (push prompt asked) "张三"))
                     "张三：https://a.test 2026-10-23 14:05 张三 {other} \\1"))
      (should (equal asked '("名字: "))))))

(ert-deftest launcher-snippet-input-default-prefills-answer ()
  "{input:X|clipboard/date/time} 用对应值预填；其他默认值原样预填；回车可改。"
  (let ((asked nil))
    (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) "https://a.test"))
              ((symbol-function 'format-time-string) (lambda (&rest _) "2026-10-29")))
      (should (equal (launcher-snippets-expand
                      "[{input:标题|无题}]({input:链接|clipboard}) {input:日期|date} {input:链接|x}"
                      (lambda (prompt initial)
                        (push (cons prompt initial) asked)
                        (if (equal prompt "日期: ") "2026-11-01" initial)))
                     "[无题](https://a.test) 2026-11-01 https://a.test"))
      (should (equal (nreverse asked)
                     '(("标题: " . "无题") ("链接: " . "https://a.test")
                       ("日期: " . "2026-10-29")))))))

(defmacro launcher-test--with-clipboard (value &rest body)
  "Run BODY with the system clipboard reading as VALUE."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) ,value)))
     ,@body))

(ert-deftest launcher-snippet-shift-time-units ()
  "偏移单位区分大小写：m 分钟、M 月；月按日历走，1 月 31 日加一月落在 2 月底。"
  (let* ((base (encode-time '(0 0 12 31 1 2026 nil -1 nil)))
         (shift (lambda (offset)
                  (format-time-string "%Y-%m-%d %H:%M"
                                      (launcher-snippets--shift-time base offset)))))
    (should (equal (funcall shift "+30m") "2026-01-31 12:30"))
    (should (equal (funcall shift "+1M") "2026-02-28 12:00"))
    (should (equal (funcall shift "+1w -1d +2h") "2026-02-06 14:00"))
    (should (equal (funcall shift "-1y") "2025-01-31 12:00"))
    (should-error (funcall shift "+1x") :type 'user-error)))

(ert-deftest launcher-snippet-date-options-and-day ()
  "{date}/{day} 接受 format 和 offset；{day} 用配置的星期名，否则用 %A。"
  (should (equal (launcher-snippets-expand "{date format=\"%Y\"}")
                 (format-time-string "%Y")))
  (should (equal (launcher-snippets-expand "{date offset=+1d}")
                 (format-time-string "%Y-%m-%d" (time-add nil 86400))))
  (let ((launcher-snippets-weekday-names ["日" "一" "二" "三" "四" "五" "六"]))
    (should (string-match-p "\\`[日一二三四五六]\\'" (launcher-snippets-expand "{day}"))))
  (should (equal (launcher-snippets-expand "{day}") (format-time-string "%A")))
  (should (string-match-p "\\`[0-9a-f-]\\{36\\}\\'" (launcher-snippets-expand "{uuid}"))))

(ert-deftest launcher-snippet-modifiers ()
  "修饰符按顺序应用；未知修饰符报错而不是悄悄忽略。"
  (launcher-test--with-clipboard "  a b\nc  "
    (should (equal (launcher-snippets-expand "{clipboard | trim | uppercase}") "A B\nC"))
    (should (equal (launcher-snippets-expand "{clipboard | oneline}") "a b c"))
    (should (equal (launcher-snippets-expand "{clipboard | trim | quote}") "> a b\n> c"))
    (should (equal (launcher-snippets-expand "{clipboard | trim | percent-encode}")
                   "a%20b%0Ac"))
    (should (equal (launcher-snippets-expand "{clipboard | trim | json-stringify}")
                   "\"a b\\nc\""))
    (should-error (launcher-snippets-expand "{clipboard | nope}") :type 'user-error))
  (should (equal (launcher-snippets-expand "{input:x|abc | uppercase}" (lambda (_ i) i))
                 "ABC")))

(ert-deftest launcher-snippet-values-are-not-rescanned ()
  "剪贴板里的占位符是普通文字，不会被再次展开、也不会触发提问。"
  (launcher-test--with-clipboard "{input:偷问} {date}"
    (should (equal (launcher-snippets-expand "{clipboard}"
                                                 (lambda (&rest _) (error "不该提问")))
                   "{input:偷问} {date}"))))

(ert-deftest launcher-snippet-input-default-takes-options ()
  "默认值可以是带选项的自动占位符。"
  (should (equal (launcher-snippets-expand "{input:截止|date offset=\"+1d\"}"
                                               (lambda (_ initial) initial))
                 (format-time-string "%Y-%m-%d" (time-add nil 86400)))))

(ert-deftest launcher-snippet-choice-and-multi ()
  "choice 按书写顺序给选项，同名只问一次；multi 用能换行的键位；都不进历史。"
  (let (seen)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt table _pred require-match _init hist default)
                 (push (list prompt (funcall table "" nil t)
                             (funcall table "" nil 'metadata) require-match hist default)
                       seen)
                 "随意")))
      (should (equal (launcher-snippets-expand "{choice:语气|正式， 随意,简短} {choice:语气|x}")
                     "随意 随意"))
      (pcase-let ((`((,prompt ,options ,metadata ,require-match ,hist ,default)) seen))
        (should (equal prompt "语气: "))
        (should (equal options '("正式" "随意" "简短")))
        (should (eq (alist-get 'display-sort-function (cdr metadata)) 'identity))
        (should (eq require-match t))
        (should (eq hist t))
        (should (equal default "正式"))))
    (should-error (launcher-snippets-expand "{choice:语气}") :type 'user-error))
  (cl-letf (((symbol-function 'read-from-minibuffer)
             (lambda (_prompt initial keymap _read hist)
               (should (eq keymap launcher-snippets-multi-map))
               (should (eq hist t))
               (concat initial "\n第二行"))))
    (should (equal (launcher-snippets-expand "{multi:正文|第一行}") "第一行\n第二行")))
  (should (eq (keymap-lookup launcher-snippets-multi-map "C-j") #'newline)))

(ert-deftest launcher-snippet-optional-section ()
  "可选段：是则保留并继续填占位符，否则连同标记所在行去掉，不留空行。"
  (let ((text "A\n{optional:加备注}\n备注：{input:内容}\n{/optional}\nB"))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
      (should (equal (launcher-snippets-expand text (lambda (&rest _) "x"))
                     "A\n备注：x\nB")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (should (equal (launcher-snippets-expand text (lambda (&rest _) (error "不该提问")))
                     "A\nB"))
      ;; 否定的答案也只问一次。
      (let ((count 0))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) (cl-incf count) nil)))
          (launcher-snippets-expand "{optional:x}a{/optional}{optional:x}b{/optional}")
          (should (= count 1))))))
  (should-error (launcher-snippets-expand "{optional:x}a") :type 'user-error))

(ert-deftest launcher-snippet-includes-other-snippets ()
  "{snippet:名字} 引用同文件模板并共享答案；缺失和循环都明确报错。"
  (let* ((file (make-temp-file "launcher-snippets" nil ".org" "\
* 签名
{input:名字}敬上
* 信
{input:名字}：{snippet:签名}
* 甲
{snippet:乙}
* 乙
{snippet:甲}
"))
         (launcher-snippets-file file)
         (count 0))
    (unwind-protect
        (progn
          (should (equal (launcher-snippets-expand "{snippet:信}"
                                                       (lambda (&rest _) (cl-incf count) "张三"))
                         "张三：张三敬上"))
          (should (= count 1))
          (should-error (launcher-snippets-expand "{snippet:没有}") :type 'user-error)
          (should-error (launcher-snippets-expand "{snippet:甲}") :type 'user-error))
      (delete-file file))))

(ert-deftest launcher-snippets-read-skips-property-drawer ()
  "标题下的属性抽屉不算模板内容；只有抽屉的标题仍被跳过。"
  (let* ((file (make-temp-file "launcher-snippets" nil ".org" "\
* 甲
:PROPERTIES:
:ID: abc
:END:
正文
* 乙
:PROPERTIES:
:ID: def
:END:
"))
         (snippets (unwind-protect (launcher-snippets--read file)
                     (delete-file file))))
    (should (equal (mapcar (lambda (s) (plist-get s :data)) snippets) '("正文")))))

(ert-deftest launcher-snippet-error-copies-nothing ()
  "模板写错时不复制、不跑复制钩子，错误留在 echo area。"
  (let ((kill-ring nil) (interprogram-cut-function nil) (copied nil) shown
        (launcher-after-copy-hook (list (lambda () (setq copied t)))))
    (cl-letf (((symbol-function 'message) (lambda (&rest args) (setq shown (apply #'format args)))))
      (funcall (launcher-snippets-submit '(:title "坏" :data "{date | nope}"))))
    (should-not kill-ring)
    (should-not copied)
    (should (string-match-p "unknown modifier" shown))))

(ert-deftest launcher-snippet-clipboard-skips-previous-result ()
  "上一个模板的结果不算用户复制的内容；用户重新复制后以新内容为准。"
  (let* ((clipboard "用户复制")
         (launcher-snippets--last-output nil) (launcher-snippets--last-copied nil)
         (run (lambda (text)
                (funcall (launcher-snippets-submit (list :title "t" :data text)))
                clipboard)))
    (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) clipboard))
              ((symbol-function 'kill-new) (lambda (text &rest _) (setq clipboard text)))
              ((symbol-function 'message) #'ignore))
      (should (equal (funcall run "甲") "甲"))
      (should (equal (funcall run "乙 {clipboard}") "乙 用户复制"))
      (should (equal (funcall run "丙 {clipboard}") "丙 用户复制"))
      (setq clipboard "新内容")
      (should (equal (funcall run "丁 {clipboard}") "丁 新内容")))))

(ert-deftest launcher-snippet-submit-defers-and-runs-copy-hook ()
  "回车只返回一个函数；调用它时才填占位符、复制，并跑复制钩子。"
  (let* ((kill-ring nil) (interprogram-cut-function nil) (copied nil)
         (launcher-after-copy-hook (list (lambda () (setq copied t)))))
    (cl-letf (((symbol-function 'gui-get-selection) (lambda (&rest _) "剪贴板"))
              ((symbol-function 'message) #'ignore))
      (let ((action (launcher-snippets-submit '(:title "引用" :data "> {clipboard}"))))
        (should (functionp action))
        (should-not kill-ring)
        (should-not (funcall action))
        (should (equal (car kill-ring) "> 剪贴板"))
        (should copied)))))

(ert-deftest launcher-snippets-tool-entry ()
  "构造出的条目带前缀、名字、图标，能通过 launcher 的校验。"
  (let ((tool (launcher-snippets-tool "t" :icon "I")))
    (should (equal (car tool) "t"))
    (should (equal (plist-get (cdr tool) :icon) "I"))
    (should (launcher--tool-from-entry tool))))

;;; launcher-snippets-tests.el ends here
