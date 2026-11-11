;;; launcher-osx-dictionary.el --- Apple Dictionary tool for launcher -*- lexical-binding: t; -*-

;;; Commentary:

;; `launcher-osx-dictionary-lookup' is a handler for `launcher-tools': it
;; looks a word up in Apple's dictionaries with the osx-dictionary package
;; (https://github.com/xuchunyang/osx-dictionary.el) and returns the
;; package's rendering of the result, without displaying it.  Register it
;; under a prefix of your choice:
;;
;;   (require 'launcher-osx-dictionary)
;;   (setq launcher-tools
;;         '(("d" :name "Dictionary"
;;                 :query launcher-osx-dictionary-query :debounce 0.2
;;                 :submit launcher-osx-dictionary-submit)))
;;
;; By default (`launcher-osx-dictionary-format' `html') a lookup renders
;; each entry's HTML with `shr', laid out by the dictionary's own style
;; sheet, and searches each enabled dictionary separately; the first one
;; builds a small helper for that (`launcher-osx-dictionary--html-helper').
;; Set the format to `text' for osx-dictionary's plain text instead.
;;
;; Loading this file loads neither osx-dictionary nor its helper; the
;; first lookup does, and reports what is missing instead of looking up.
;; Needs macOS, osx-dictionary and Xcode's command line tools, with which
;; the first lookup builds the package's helper if it is missing.
;;
;; Tested with osx-dictionary commit 655bca5 (2026-09-06).  It has no
;; public function that looks a word up without prompting or displaying,
;; so this file uses its private `osx-dictionary--insert-search-result',
;; `osx-dictionary--current-dictionary-description',
;; `osx-dictionary--current-word' and `osx-dictionary--load-dir', and
;; reports a version that lacks them.

;;; Code:

(require 'launcher)
(require 'nadvice)
(require 'url-util)
(require 'cl-lib)
(require 'dom)
(require 'shr)

(defvar osx-dictionary-cli)
(defvar osx-dictionary--load-dir)
(defvar osx-dictionary--current-word)
(defvar osx-dictionary-current-dictionary)
(defvar osx-dictionary-search-log-file)
(declare-function osx-dictionary-mode "osx-dictionary" ())
(declare-function osx-dictionary--insert-search-result "osx-dictionary" (word))
(declare-function osx-dictionary--current-dictionary-description "osx-dictionary" ())
(declare-function osx-dictionary-select-dictionary "osx-dictionary" (&optional dictionary))
(declare-function osx-dictionary--display-name-for "osx-dictionary" (real-name))
(defvar osx-dictionary-allowed-dictionaries)
(defvar osx-dictionary-separator)

(defcustom launcher-osx-dictionary-buffer-name "*Launcher Dictionary*"
  "Name of the buffer `launcher-osx-dictionary-lookup' shows results in.
It differs from osx-dictionary's own buffer, so that lookups from the
launcher leave the package's own session alone."
  :type 'string
  :group 'launcher)

(defconst launcher-osx-dictionary--functions
  '(osx-dictionary-mode osx-dictionary--insert-search-result
    osx-dictionary--current-dictionary-description osx-dictionary-select-dictionary)
  "Functions of osx-dictionary this file uses.")

(defconst launcher-osx-dictionary--variables
  '(osx-dictionary-cli osx-dictionary--load-dir osx-dictionary--current-word
    osx-dictionary-current-dictionary)
  "Variables of osx-dictionary this file uses.")

(defvar-local launcher-osx-dictionary--speech-process nil
  "Speech process for this result buffer.")

(defvar-local launcher-osx-dictionary--result-prefix nil
  "Launcher tool prefix that produced the displayed result.")

(defun launcher-osx-dictionary--query-prefix ()
  "Return the prefix of the active launcher query, if any."
  (when launcher--query-session
    (when-let* ((route (launcher--query-session-route launcher--query-session)))
      (launcher--tool-prefix (car route)))))

(defun launcher-osx-dictionary--require ()
  "Load osx-dictionary, or signal an error saying what is missing."
  (unless (eq system-type 'darwin)
    (error "Apple dictionaries are only available on macOS"))
  (unless (require 'osx-dictionary nil t)
    (error "Needs the osx-dictionary package: M-x package-install RET osx-dictionary"))
  (when-let* ((missing (append (seq-remove #'fboundp launcher-osx-dictionary--functions)
                              (seq-remove #'boundp launcher-osx-dictionary--variables))))
    (error "This osx-dictionary lacks %s; launcher-osx-dictionary was tested with commit 655bca5"
           (mapconcat #'symbol-name missing ", "))))

(defun launcher-osx-dictionary--find-helper ()
  "Return the osx-dictionary helper, where the package looks for it, or nil."
  (or (executable-find (expand-file-name osx-dictionary-cli osx-dictionary--load-dir))
      (executable-find osx-dictionary-cli)))

(defun launcher-osx-dictionary--developer-tools-p ()
  "Return non-nil if Xcode's command line tools are installed.
Without them, Apple's clang stub would offer to install them instead."
  (and (executable-find "xcode-select")
       (eql 0 (call-process "xcode-select" nil nil nil "-p"))))

(defun launcher-osx-dictionary--build-helper ()
  "Build the osx-dictionary helper where the package looks for it first.
Return its file name, or signal an error saying why it could not."
  (let ((source (expand-file-name "osx-dictionary.m" osx-dictionary--load-dir))
        (output (expand-file-name osx-dictionary-cli osx-dictionary--load-dir)))
    (unless (file-readable-p source)
      (error "The osx-dictionary helper source %s is missing; reinstall the package"
             (abbreviate-file-name source)))
    (unless (and (executable-find "clang") (launcher-osx-dictionary--developer-tools-p))
      (error "Building the osx-dictionary helper needs the Xcode command line tools: \
run xcode-select --install"))
    (unless (file-writable-p output)
      (error "Cannot build the osx-dictionary helper: %s is not writable"
             (abbreviate-file-name osx-dictionary--load-dir)))
    (message "Building the osx-dictionary helper...")
    (with-temp-buffer
      ;; The command of `osx-dictionary-recompile', which hides failures.
      (let ((status (call-process "clang" nil t nil "-O3"
                                  "-framework" "CoreServices" "-framework" "Foundation"
                                  source "-o" output)))
        (unless (and (eql status 0) (file-executable-p output))
          (error "Building the osx-dictionary helper failed (%s): %s" status
                 (string-trim (buffer-substring (point-min)
                                                (min (point-max) 1000)))))))
    (message "Building the osx-dictionary helper...done")
    output))

(defun launcher-osx-dictionary--dictionaries (helper)
  "Return the dictionaries HELPER lists, or signal an error if it fails."
  (with-temp-buffer
    (let ((status (call-process helper nil '(t nil) nil "-l")))
      (unless (eql status 0)
        (error "The osx-dictionary helper failed (%s)" status))
      (split-string (buffer-string) "\n" t))))

(defun launcher-osx-dictionary--no-result (word helper)
  "Signal an error saying why HELPER found nothing for WORD."
  (let ((installed (launcher-osx-dictionary--dictionaries helper)))
    (cond ((null installed)
           (error "Dictionary.app lists no installed dictionaries"))
          ((and osx-dictionary-current-dictionary
                (not (member osx-dictionary-current-dictionary installed)))
           (error "Dictionary %S is not installed; choose another with M-x \
osx-dictionary-select-dictionary" osx-dictionary-current-dictionary))
          (t
           (error "No definition for %S (searched: %s)" word
                  (osx-dictionary--current-dictionary-description))))))

(defcustom launcher-osx-dictionary-format 'html
  "How lookups render definitions.
`html' renders each entry's HTML with `shr', laid out as the
dictionary's own style sheet lays it out (headwords, senses and examples
on lines of their own), searching each dictionary separately.  `text'
uses osx-dictionary's plain text, in which an entry's parts run together."
  :type '(choice (const :tag "Entry HTML, rendered" html)
                 (const :tag "osx-dictionary's plain text" text))
  :group 'launcher)

(defcustom launcher-osx-dictionary-html-width 78
  "Column the `html' format fills definitions to."
  :type 'natnum
  :group 'launcher)

(defconst launcher-osx-dictionary--html-helper-source "// launcher-dictionary-html: print Apple dictionary records as HTML.
// Use: launcher-dictionary-html [-d NAME]... WORD
// Without -d, search each dictionary enabled in Dictionary.app.
// Each record: a line \"\\x1dNAME\", then the record's HTML, then a newline.
#import <Foundation/Foundation.h>
#import <CoreServices/CoreServices.h>

extern CFArrayRef DCSCopyAvailableDictionaries(void);
extern CFStringRef DCSDictionaryGetName(DCSDictionaryRef dictionary);
extern CFURLRef DCSDictionaryGetURL(DCSDictionaryRef dictionary);
extern CFArrayRef DCSCopyRecordsForSearchString(DCSDictionaryRef dictionary,
                                                CFStringRef string, void *, void *);
extern CFStringRef DCSRecordCopyData(CFTypeRef record, long version);

int main(int argc, char *argv[]) {
  @autoreleasepool {
    NSMutableArray *names = [NSMutableArray array];
    int i = 1;
    while (i + 1 < argc && strcmp(argv[i], \"-d\") == 0) {
      [names addObject:[NSString stringWithUTF8String:argv[i + 1]]];
      i += 2;
    }
    if (i >= argc || argv[i][0] == '\\0') return 2;
    NSString *word = [NSString stringWithUTF8String:argv[i]];
    NSArray *available = (NSArray *)DCSCopyAvailableDictionaries();
    NSMutableArray *dicts = [NSMutableArray array];
    if ([names count] > 0) {
      for (NSString *name in names)
        for (id dict in available)
          if ([(NSString *)DCSDictionaryGetName((DCSDictionaryRef)dict) isEqualToString:name])
            [dicts addObject:dict];
    } else {
      NSDictionary *prefs =
          [[NSUserDefaults standardUserDefaults] persistentDomainForName:@\"com.apple.DictionaryServices\"];
      for (NSString *path in prefs[@\"DCSActiveDictionaries\"]) {
        NSString *wanted = [[path stringByResolvingSymlinksInPath] lastPathComponent];
        for (id dict in available) {
          NSURL *url = (NSURL *)DCSDictionaryGetURL((DCSDictionaryRef)dict);
          if ([[[url path] lastPathComponent] isEqualToString:wanted]) {
            [dicts addObject:dict];
            break;
          }
        }
      }
    }
    for (id dict in dicts) {
      NSString *name = (NSString *)DCSDictionaryGetName((DCSDictionaryRef)dict);
      NSArray *records = (NSArray *)DCSCopyRecordsForSearchString(
          (DCSDictionaryRef)dict, (CFStringRef)word, NULL, NULL);
      for (id record in records) {
        NSString *html = (NSString *)DCSRecordCopyData((CFTypeRef)record, 1);
        if ([html length] == 0) continue;
        printf(\"\\x1d%s\\n%s\\n\", [name UTF8String], [html UTF8String]);
      }
    }
  }
  return 0;
}
"
  "Source of the helper that prints dictionary entries as HTML.")

(defconst launcher-osx-dictionary--html-helper-name "launcher-dictionary-html-1"
  "File name of the built HTML helper; the suffix changes with its source.")

(defcustom launcher-osx-dictionary-html-helper-directory
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "Directory the first `html' lookup builds the HTML helper in."
  :type 'directory
  :group 'launcher)

(defun launcher-osx-dictionary--html-helper ()
  "Return the HTML helper, building it first if it is missing.
Signal an error saying why it could not be built."
  (let ((output (expand-file-name launcher-osx-dictionary--html-helper-name
                                  launcher-osx-dictionary-html-helper-directory)))
    (unless (file-executable-p output)
      (unless (and (executable-find "clang") (launcher-osx-dictionary--developer-tools-p))
        (error "Building the dictionary HTML helper needs the Xcode command line tools: \
run xcode-select --install"))
      (unless (file-writable-p output)
        (error "Cannot build the dictionary HTML helper: %s is not writable"
               (abbreviate-file-name launcher-osx-dictionary-html-helper-directory)))
      (let ((source (make-temp-file "launcher-dictionary-html" nil ".m"
                                    launcher-osx-dictionary--html-helper-source)))
        (unwind-protect
            (with-temp-buffer
              (let ((status (call-process "clang" nil t nil "-O2"
                                          "-framework" "CoreServices" "-framework" "Foundation"
                                          source "-o" output)))
                (unless (and (eql status 0) (file-executable-p output))
                  (error "Building the dictionary HTML helper failed (%s): %s" status
                         (string-trim (buffer-substring (point-min)
                                                        (min (point-max) 1000)))))))
          (delete-file source))))
    output))

(defun launcher-osx-dictionary--html-dictionaries ()
  "Return the dictionary names to search, as osx-dictionary is set, or nil.
Nil means every dictionary enabled in Dictionary.app."
  (cond (osx-dictionary-current-dictionary
         (list osx-dictionary-current-dictionary))
        ((bound-and-true-p osx-dictionary-allowed-dictionaries)
         (mapcar (lambda (entry) (if (consp entry) (car entry) entry))
                 osx-dictionary-allowed-dictionaries))))

(defun launcher-osx-dictionary--html-records (helper word)
  "Return the entries HELPER finds for WORD, as (DICTIONARY . HTML) pairs."
  (with-temp-buffer
    (let ((status (apply #'call-process helper nil '(t nil) nil
                         (append (mapcan (lambda (name) (list "-d" name))
                                         (launcher-osx-dictionary--html-dictionaries))
                                 (list word)))))
      (unless (eql status 0)
        (error "The dictionary HTML helper failed (%s)" status))
      (mapcar (lambda (record)
                (let ((newline (or (string-search "\n" record) (length record))))
                  (cons (substring record 0 newline)
                        (substring record (min (length record) (1+ newline))))))
              (split-string (buffer-string) "\x1d" t)))))

;; Dictionary entries are mostly nested spans; their style sheets decide
;; which parts start lines and which are hidden.  Only `display' rules
;; matter here, matched approximately: a selector is a chain of compounds
;; (TAG, .CLASS or TAG.CLASS...) joined by descendant or `>' combinators,
;; matched greedily, and a selector using anything else is ignored.

(defvar launcher-osx-dictionary--css-cache (make-hash-table :test #'equal)
  "Display rules of each dictionary's style sheet, by style sheet text.")

(defun launcher-osx-dictionary--css-compound (string)
  "Return STRING, a simple compound selector, as (TAG . CLASSES), or nil."
  (when (string-match "\\`\\([a-zA-Z][a-zA-Z0-9]*\\)?\\(\\(?:\\.[-_a-zA-Z0-9]+\\)*\\)\\'" string)
    (let ((tag (match-string 1 string))
          (classes (split-string (match-string 2 string) "\\." t)))
      (when (or tag classes)
        (cons (and tag (intern (downcase tag))) classes)))))

(defun launcher-osx-dictionary--css-drop-at-blocks (css)
  "Return CSS without its @-rule blocks, such as @media print {...}."
  (with-temp-buffer
    (insert css)
    (goto-char (point-min))
    (while (re-search-forward "@[^{};]*{" nil t)
      (let ((start (match-beginning 0)) (depth 1))
        (while (and (> depth 0) (re-search-forward "[{}]" nil 'move))
          (setq depth (if (eq (char-before) ?{) (1+ depth) (1- depth))))
        (delete-region start (point))))
    (buffer-string)))

(defun launcher-osx-dictionary--css-rules (css)
  "Return the `display' rules of CSS, a style sheet, in source order.
Each is (COMPOUNDS SPECIFICITY DISPLAY), COMPOUNDS rightmost first; each
compound after the first is (CHILD TAG . CLASSES), CHILD non-nil when it
must be the parent of the compound before it."
  (or (gethash css launcher-osx-dictionary--css-cache)
      (puthash
       css
       (let ((css (launcher-osx-dictionary--css-drop-at-blocks
                   (replace-regexp-in-string "/\\*\\(?:.\\|\n\\)*?\\*/" "" css)))
             (start 0) rules)
         (while (string-match "\\([^{}]+\\){\\([^{}]*\\)}" css start)
           (setq start (match-end 0))
           (let ((selectors (match-string 1 css))
                 (body (match-string 2 css)))
             (when (string-match "display[ \t\n]*:[ \t\n]*\\([-a-z]+\\)" body)
               (let ((display (intern (match-string 1 body))))
                 (dolist (selector (split-string selectors ","))
                   (let* ((parts (split-string (replace-regexp-in-string ">" " > " selector)))
                          (compounds nil) (child nil) (valid parts))
                     (dolist (part parts)
                       (if (equal part ">")
                           (setq child t)
                         (let ((compound (launcher-osx-dictionary--css-compound part)))
                           (unless compound (setq valid nil))
                           (push (cons child compound) compounds)
                           (setq child nil))))
                     ;; COMPOUNDS is rightmost first, each with the combinator
                     ;; to its left; matching right to left, that combinator
                     ;; relates it to the next compound, so shift it there.
                     (setq compounds
                           (cons (cdar compounds)
                                 (cl-loop for prev in compounds
                                          for c in (cdr compounds)
                                          collect (cons (car prev) (cdr c)))))
                     (when (and valid compounds)
                       (push (list compounds
                                   (apply #'+ (mapcar (lambda (c)
                                                        (+ (* 10 (length (cdr c)))
                                                           (if (car c) 1 0)))
                                                      (cons (car compounds)
                                                            (mapcar #'cdr (cdr compounds)))))
                                   display)
                             rules))))))))
         (nreverse rules))
       launcher-osx-dictionary--css-cache)))

(defun launcher-osx-dictionary--css-match-p (compound node)
  "Return non-nil if NODE, a DOM element, matches COMPOUND."
  (and (or (null (car compound)) (eq (car compound) (dom-tag node)))
       (let ((classes (split-string (or (dom-attr node 'class) ""))))
         (seq-every-p (lambda (class) (member class classes)) (cdr compound)))))

(defun launcher-osx-dictionary--css-display (rules node ancestors)
  "Return the `display' RULES give NODE, whose ANCESTORS are innermost first."
  (let (best best-specificity)
    (dolist (rule rules)
      (pcase-let ((`(,compounds ,specificity ,display) rule))
        (when (and (launcher-osx-dictionary--css-match-p (car compounds) node)
                   (let ((rest (cdr compounds)) (up ancestors) (failed nil))
                     (while (and rest up (not failed))
                       (pcase-let ((`(,child . ,compound) (car rest)))
                         (cond ((launcher-osx-dictionary--css-match-p compound (car up))
                                (setq rest (cdr rest)))
                               (child (setq failed t))))
                       (setq up (cdr up)))
                     (and (not failed) (null rest)))
                   (or (null best) (>= specificity best-specificity)))
          (setq best display best-specificity specificity))))
    best))

(defun launcher-osx-dictionary--css-apply (rules node &optional ancestors)
  "Return a copy of NODE laid out by RULES for `shr', or nil to drop it.
Hidden elements are dropped; block-level ones become `div', inline
ones `span'.  ANCESTORS are NODE's, innermost first."
  (if (not (consp node))
      node
    (let ((tag (dom-tag node)))
      (unless (memq tag '(head style script comment))
        (let ((display (launcher-osx-dictionary--css-display rules node ancestors)))
          (unless (eq display 'none)
            (let ((children (delq nil (mapcar (lambda (child)
                                                (launcher-osx-dictionary--css-apply
                                                 rules child (cons node ancestors)))
                                              (dom-children node)))))
              `(,(cond ((memq display '(block list-item table flex grid)) 'div)
                       ((memq display '(inline inline-block)) 'span)
                       (t tag))
                ,(dom-attributes node)
                ,@children))))))))

(defun launcher-osx-dictionary--render-html (html)
  "Return HTML, one dictionary entry, rendered as text by `shr'."
  (let* ((dom (with-temp-buffer
                (insert html)
                (libxml-parse-html-region (point-min) (point-max))))
         (css (mapconcat (lambda (style)
                           (apply #'concat (seq-filter #'stringp (dom-children style))))
                         (dom-by-tag dom 'style) "\n"))
         (rules (launcher-osx-dictionary--css-rules css))
         (body (or (car (dom-by-tag dom 'body)) dom)))
    (with-temp-buffer
      (let ((shr-width launcher-osx-dictionary-html-width)
            (shr-use-fonts nil)
            (shr-inhibit-images t)
            (shr-use-colors nil))
        (shr-insert-document (launcher-osx-dictionary--css-apply rules body)))
      (string-trim (buffer-string)))))

(defun launcher-osx-dictionary--search-html (word)
  "Return WORD's definitions rendered from their HTML, as a string, or nil."
  (when-let* ((records (launcher-osx-dictionary--html-records
                        (launcher-osx-dictionary--html-helper) word)))
    (let (last)
      (with-temp-buffer
        (pcase-dolist (`(,dictionary . ,html) records)
          (if (equal dictionary last)
              (insert osx-dictionary-separator)
            (unless (bobp) (insert "\n"))
            (insert (propertize (if (fboundp 'osx-dictionary--display-name-for)
                                    (osx-dictionary--display-name-for dictionary)
                                  dictionary)
                                'font-lock-face 'osx-dictionary-dictionary-name)
                    "\n")
            (setq last dictionary))
          (insert (launcher-osx-dictionary--render-html html) "\n"))
        (buffer-string)))))

(defun launcher-osx-dictionary--search (word)
  "Return WORD's definitions as a string, in `launcher-osx-dictionary-format'.
Signal an error if it finds none."
  (or (and (eq launcher-osx-dictionary-format 'html)
           (launcher-osx-dictionary--search-html word))
      (launcher-osx-dictionary--search-text word)))

(defun launcher-osx-dictionary--search-text (word)
  "Return osx-dictionary's rendering of WORD's definitions, as a string.
Signal an error if it finds none."
  (let ((helper (or (launcher-osx-dictionary--find-helper)
                    (launcher-osx-dictionary--build-helper))))
    (with-temp-buffer
      ;; Rendered as `osx-dictionary--view-result' does, but here.
      (osx-dictionary--insert-search-result word)
      (when (string-blank-p (buffer-string))
        (launcher-osx-dictionary--no-result word helper))
      (whitespace-cleanup)
      (buffer-string))))

(defvar-keymap launcher-osx-dictionary-result-mode-map
  :doc "Keys of launcher's dictionary results, over `osx-dictionary-mode-map'."
  "q" #'launcher-osx-dictionary-quit
  "s" #'launcher-osx-dictionary-search
  "S" #'launcher-osx-dictionary-select-dictionary
  "r" #'launcher-osx-dictionary-read-word
  "o" #'launcher-osx-dictionary-open-dictionary)

(define-minor-mode launcher-osx-dictionary-result-mode
  "Minor mode of launcher's dictionary results.
It replaces the `osx-dictionary-mode' keys that would display the
package's own buffer, or restore the window configuration it saved,
in this buffer only, and provides asynchronous speech and URL opening:
\\{launcher-osx-dictionary-result-mode-map}"
  :lighter nil)

(defun launcher-osx-dictionary--render (word text &optional prefix)
  "Show TEXT, the definitions of WORD, in the result buffer and return it.
Display nothing.  Remember the originating tool PREFIX for further searches."
  (let ((buffer (get-buffer-create launcher-osx-dictionary-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'osx-dictionary-mode)
        (osx-dictionary-mode))
      ;; User hooks can enable View mode for read-only buffers.  Its
      ;; minor-mode map shadows dictionary keys (notably r with backward
      ;; isearch); the dictionary mode already provides read-only results.
      (when (bound-and-true-p view-mode)
        (view-mode -1))
      (launcher-osx-dictionary--stop-speech)
      (add-hook 'kill-buffer-hook #'launcher-osx-dictionary--stop-speech nil t)
      ;; These generated results have no edits to undo or save.
      (setq buffer-undo-list t)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      ;; For the package's commands that act on the word, such as `o'.
      (setq osx-dictionary--current-word word)
      (setq launcher-osx-dictionary--result-prefix prefix)
      (launcher-osx-dictionary-result-mode 1)
      (goto-char (point-min))
      (dolist (window (get-buffer-window-list buffer nil t))
        (set-window-point window (point-min))
        (set-window-start window (point-min))))
    buffer))

(defun launcher-osx-dictionary--text (word &optional preview)
  "Return definitions for trimmed WORD without changing a result buffer.
With PREVIEW, leave the package's search log untouched."
  (launcher-osx-dictionary--require)
  (let* ((osx-dictionary-search-log-file
          (and (not preview) osx-dictionary-search-log-file
               (expand-file-name osx-dictionary-search-log-file)))
         (default-directory (file-name-as-directory osx-dictionary--load-dir)))
    (launcher-osx-dictionary--search word)))

;;;###autoload
(defun launcher-osx-dictionary-query (query)
  "Return a definition preview for QUERY, without displaying or rendering a buffer.
Use this as a tool's :query with :debounce 0.2.  Selecting the candidate
renders the captured definitions without looking the word up again."
  (let ((word (string-trim query)))
    (unless (string-empty-p word)
      (let* ((prefix (launcher-osx-dictionary--query-prefix))
             (text (launcher-osx-dictionary--text word t))
             (log-file (and osx-dictionary-search-log-file
                            (expand-file-name osx-dictionary-search-log-file)))
             (summary (truncate-string-to-width
                       (string-join (split-string (substring-no-properties text) "[[:space:]]+" t) " ")
                       160 nil nil "…")))
        (list (list :id word :title word :subtitle summary
                    :data (list :word word :text text :prefix prefix :log-file log-file)))))))

;;;###autoload
(defun launcher-osx-dictionary-submit (candidate)
  "Render CANDIDATE's captured definition without another lookup."
  (let* ((data (plist-get candidate :data))
         (word (plist-get data :word))
         (log-file (plist-get data :log-file)))
    (when log-file (append-to-file (concat word "\n") nil log-file))
    (launcher-osx-dictionary--render word (plist-get data :text)
                                    (plist-get data :prefix))))

;;;###autoload
(defun launcher-osx-dictionary-lookup (query)
  "Look QUERY up in Apple's dictionaries; return the result buffer.
A handler for `launcher-tools'.  Look up QUERY without surrounding
whitespace, in the dictionaries osx-dictionary is set to search, and
render the result as the package does, in the buffer named by
`launcher-osx-dictionary-buffer-name', without displaying it.  The
buffer is in `osx-dictionary-mode', with the keys of
`launcher-osx-dictionary-result-mode'.

Signal an error, leaving the buffer as it was, if osx-dictionary or
its helper is missing or fails, or if there is no definition.  Build
the helper first if it is missing, which blocks Emacs meanwhile, as
does the lookup."
  (let ((word (string-trim query)))
    (when (string-empty-p word)
      (error "Nothing to look up"))
    (launcher-osx-dictionary--render word (launcher-osx-dictionary--text word)
                                     (launcher-osx-dictionary--query-prefix))))

(defun launcher-osx-dictionary-quit ()
  "Quit the result window without restoring the package's saved layout."
  (interactive)
  (launcher-osx-dictionary--stop-speech)
  (quit-window))

(defun launcher-osx-dictionary--stop-speech ()
  "Stop any speech belonging to this result buffer."
  (when (and (processp launcher-osx-dictionary--speech-process)
             (process-live-p launcher-osx-dictionary--speech-process))
    (delete-process launcher-osx-dictionary--speech-process))
  (setq launcher-osx-dictionary--speech-process nil))

(defun launcher-osx-dictionary--word ()
  "Return the displayed query, or report that there is no result."
  (unless (and (boundp 'osx-dictionary--current-word)
               (stringp osx-dictionary--current-word)
               (not (string-blank-p osx-dictionary--current-word)))
    (user-error "No dictionary word to act on"))
  osx-dictionary--current-word)

(defun launcher-osx-dictionary-read-word ()
  "Read the displayed word asynchronously; repeated calls restart speech."
  (interactive)
  (let ((word (launcher-osx-dictionary--word)))
    (unless (executable-find "say")
      (user-error "The macOS speech command `say' is unavailable"))
    (launcher-osx-dictionary--stop-speech)
    (setq launcher-osx-dictionary--speech-process
          (make-process :name "launcher-dictionary-speech" :buffer nil
                        :command '("say" "-f" "-") :connection-type 'pipe
                        :coding 'utf-8-unix :noquery t
                        :sentinel (lambda (process _event)
                                    (when (and (eq (process-status process) 'exit)
                                               (not (zerop (process-exit-status process))))
                                      (message "Dictionary speech failed (%s)"
                                               (process-exit-status process))))))
    (process-send-string launcher-osx-dictionary--speech-process (concat word "\n"))
    (process-send-eof launcher-osx-dictionary--speech-process)))

(defun launcher-osx-dictionary-open-dictionary ()
  "Open the displayed word or phrase in Dictionary.app."
  (interactive)
  (let ((word (launcher-osx-dictionary--word)))
    (unless (executable-find "open")
      (user-error "The macOS command `open' is unavailable"))
    (make-process :name "launcher-dictionary-open" :buffer nil :noquery t
                  :sentinel (lambda (process _event)
                              (when (and (eq (process-status process) 'exit)
                                         (not (zerop (process-exit-status process))))
                                (message "Opening Dictionary.app failed (%s)"
                                         (process-exit-status process))))
                  :command (list "open" (concat "dict://" (url-hexify-string word))))))

(defun launcher-osx-dictionary-search ()
  "Return to launcher with the configured dictionary prefix ready for input."
  (interactive)
  (let* ((tools (seq-filter
                 (lambda (tool)
                   (and (launcher--tool-prefix tool)
                        (or (eq (launcher--tool-query tool) #'launcher-osx-dictionary-query)
                            (eq (launcher--tool-submit tool) #'launcher-osx-dictionary-submit))))
                 (launcher--tools)))
         (tool (or (seq-find (lambda (tool)
                              (equal (launcher--tool-prefix tool)
                                     launcher-osx-dictionary--result-prefix))
                            tools)
                   (car tools))))
    (unless tool
      (user-error "Configure a prefixed dictionary tool in `launcher-tools' first"))
    (launcher nil (concat (launcher--tool-prefix tool) " "))))

(defun launcher-osx-dictionary-select-dictionary ()
  "Choose the dictionaries to search, then look this word up again here.
Prompt as `osx-dictionary-select-dictionary', which saves the choice
for later lookups, but show the word in this buffer, not in the
package's own."
  (interactive)
  (let* ((word (launcher-osx-dictionary--word))
         (prefix launcher-osx-dictionary--result-prefix)
         (default-directory (file-name-as-directory osx-dictionary--load-dir))
         (args (advice-eval-interactive-spec
                (cadr (interactive-form #'osx-dictionary-select-dictionary)))))
    ;; Outside its mode, the command does not show the word itself.
    (with-temp-buffer
      (apply #'osx-dictionary-select-dictionary args))
    (launcher-osx-dictionary--render word (launcher-osx-dictionary--text word) prefix)))

(provide 'launcher-osx-dictionary)
;;; launcher-osx-dictionary.el ends here
