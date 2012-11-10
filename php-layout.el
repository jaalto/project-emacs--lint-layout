;;; lint-layout.el --- Coding layout lint utilities.

;;{{{ Id

;; Copyright (C)    2006-2009 Jari Aalto <jari.aalto@cante.net>
;; Keywords:        extensions
;; Author:          Jari Aalto
;; Maintainer:      Jari Aalto
;;
;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with program. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
;;
;; Visit <http://www.gnu.org/copyleft/gpl.html> for more information

;;}}}
;;{{{ Install

;; ........................................................ &t-install ...
;;   Put this file on your Emacs-Lisp `load-path', add following into your
;;   $HOME/.emacs startup file
;;
;;      (require 'lint-layout)
;;
;;   If you have any questions about this Emacs package:
;;
;;      M-x mail send question, feedback, bugs

;;}}}
;;{{{ Documentation

;; ..................................................... &t-commentary ...

;;; Commentary:

;;  Overview of features
;;
;;      This package contains utilities for static code checking, usually
;;      known by term 'linting'. See Wikipedia
;;      <http://en.wikipedia.org/wiki/Lint_programming_tool> for more
;;      about the static code checking methodology.
;;
;;      The checks include typical code conventions, most of which are
;;      configurable:
;;
;;      o   Maximum code column 80
;;      o   Extra whitespace at end of lines (spaces and tabs)
;;      o   Mixed SPACE+TAB indentation
;;      o   Brace placement (Allman lined-up; K&R)
;;      o   Indentation multiple of 4.
;;      o   Terminating semicolon checks: no loose "semicolons ;"
;;
;;      Some of the SQL checks include:
;;
;;      o   FIXME: todo
;;
;;      Some of the CSS checks include:
;;
;;      o   FIXME: todo
;;
;;      Some of the PHP checks include:
;;
;;      o   The use and definition of function or methods:
;;
;;          function name () // in definition <neme> surrounding space
;;          {
;;              call( $param, $param); // leading space in muti-arg calls
;;              call(aram);            // single arg call
;;          }
;;
;;      o   Readable keywords `and' and `or' preferred over
;;          traditional && and ||.
;;      o   Literal keywords in lowercase: false, true, null
;;      o   Multiline concat dot-operator(.) could be written using
;;          <<<HERE documents.
;;      o   Instead echo(), print() is suggested due to function to exist
;;          in all programming languages.
;;      o   Instead of date(), POSIX standard strftime() preferred.
;;
;; User callable functions (M-x):
;;
;;      Normally functions start scanning from current point foward, unless
;;      "buffer" is mentioned:
;;
;;          ;; Decides correct test set for *.css, *.php, *.sql file
;;          my-lint-layout-check-generic-buffer
;;          my-lint-layout-check-generic-file
;;          my-lint-layout-check-generic-directory
;;
;;          my-lint-layout-php-check-all-interactive
;;          my-lint-layout-php-check-phpdoc-interactive
;;          my-lint-layout-php-check-regexp-occur-buffer-interactive
;;
;;          my-lint-layout-java-check-all-interactive
;;          my-lint-layout-java-check-phpdoc-interactive
;;          my-lint-layout-java-check-regexp-occur-buffer-interactive
;;
;;          my-lint-layout-check-whitespace-buffer-interactive
;;          my-lint-layout-check-line-length-buffer-interactive
;;          my-lint-layout-check-eof-marker-interactive
;;
;;          my-lint-layout-css-check-buffer-interactive
;;          my-lint-layout-sql-buffer-interactive
;;
;;      Look at results in `my-lint-layout-changelog-check-main' buffer which
;;      by default is `my-lint-layout-buffer-name'.
;;
;; Batch command line usage
;;
;;      This lisp library canbe called from command line with list of files
;;      to check:
;;
;;          emacs -Q -q -l lint-layout.el -f

(require 'regexp-opt)

(eval-when-compile
  (require 'cl))

(defvar my-lint-layout-debug nil
  "Non-nil to turn on debug messages.")

(defconst my-lint-layout-buffer-name "*Lint checks*"
  "*Buffer name for results.")

(defconst my-lint-layout-generic-line-length-max 80
  "*Maximum line length.")

(defconst my-lint-layout-generic-indent-step 4
  "*Indent step.")

(defconst my-lint-layout-generic-brace-style 'brace-end
  "*Brace style. A symbol of lined-up or brace-end.")

(defconst my-lint-layout-generic-assignment-line-up-treshold 5
  "Number of characters apart, that assignments should be lined up.
If value is nil, line up is always checked.

An example:

    var = 100;
    variable = 100;

With threshold value 5, the assignments are within it and the '='
tokens could be lined up.")

(defconst my-lint-layout-generic-access-modifier-regexp
  (concat
   "\\<"
   (regexp-opt
    '("public"
      "protected"
      "private")
    t)
   "\\>")
  "Access modifiers.")

(defconst my-lint-layout-generic-vartype-modifier-regexp
  (concat
   "\\<"
   (regexp-opt
    '("byte"
      "short"
      "int"
      "long"
      "float"
      "double"
      "char"
      "String")
    t)
   "\\>")
  "Variale type modifiers.")

(defconst my-lint-layout-generic-other-modifier-list
  '("abstract"
    "static"
    "final"
    "const")
  "List of function, method or variable modifiers.")

(eval-when-compile
;;  Need this function at load-time
(defun my-lint-layout-make-word-sequence-regexp (list)
  "From LIST make regrep '(A|B...)?(AB|B...)' x (length LIST).
The idea is to spell out all word combinations:

 '(a b)  => (?:\b(a|b|c)\b[ \]*)?(?:\b(a|b|c)\b[ \]*)?"
  (let* ((len (length list))
	 (i   0)
	 (re  (concat
	       "\\(?:\\<"
	       (regexp-opt list t)
	       "\\>[ \t]*\\)?"))
	 regexp)
    (while (< i len)
      (incf i)
      (setq regexp
	    (concat
	     (or regexp "")
	     re)))
    regexp)))

(defconst my-lint-layout-generic-other-modifier-regexp
  (my-lint-layout-make-word-sequence-regexp
   my-lint-layout-generic-other-modifier-list)
  "Regexp of `my-lint-layout-generic-other-modifier-list'.
Notice that this is combination regexp, that does not require the
type modifiers to be present:

  <regexp>?name()")

(defconst my-lint-layout-generic-class-statement
  (regexp-opt
   '("\\(?:\\<abstract[ \t]+\\)?class"
     "interface")
   'words)
  "Class statement keyword regexp.")

(defconst my-lint-layout-generic-control-statement-start-regexp
  (regexp-opt
   '("if"
     "while"
     "switch"
     "do"
     "for"
     "foreach"
     "try")
   'words)
  "Control statement keyword regexp.")

(defconst my-lint-layout-generic-control-statement-continue-regexp
  (regexp-opt
   '("else"
     "elsif"
     "elseif"
     "else[ \t]+if"
     "catch"
     "finally")
   'words)
  "Control statement continue keyword regexp.")

(defconst my-lint-layout-generic-control-statement-regexp
  (concat
   my-lint-layout-generic-control-statement-start-regexp
   "\\|"
   my-lint-layout-generic-control-statement-continue-regexp)
  "Control statement keyword.")

;; Todo: multiline IF
;;
;;        if ( isset ( $_POST['login'] )
;;             AND ( $_POST['username'] == ""
;;                   OR $_POST['passwd'] == "") )
;;        {
;;
(defconst my-lint-layout-generic-statement-regexp-brace
  (concat
   "^"
   "\\([ \t]*\\)"  ;; Indent, submatch 1
   "\\("
       my-lint-layout-generic-class-statement
       "\\|"
       my-lint-layout-generic-control-statement-regexp
       "\\|"
       "\\(?:" my-lint-layout-generic-access-modifier-regexp
	       "[ \t]+\\)[^(;]*("
   "\\)"
   ".*"
   "\\([ \t\r\n]*{\\|[ \t].*(.*)[ \t\r\n]*{\\)")
  "Left anchored statement with brace.")

(defconst my-lint-layout-generic-statement-regexp-line
  (concat
   "^"
   "\\([ \t]*\\)"
   "\\("
   my-lint-layout-generic-control-statement-regexp
   "\\|function"
   "\\)"
   "\\([ \t].*(.*)[ \t\r\n]*\\|[ \t\r\n]*\\)")
  "Left anchored statement.
Same `my-lint-layout-generic-statement-regexp-brace' but
without brace requirement.")

(defconst my-lint-layout-generic-xml-tag-regexp
  "^[ \t]*<[?]\\|[?]>"
  "xml tag: starting, closing.")

;;; .................................................. &java-variables ...

(defvar my-lint-layout-java-variable-literals
  (concat
   "\\<"
   (regexp-opt
    '("false"
      "true"
      "null"))
   "\\>")
  "Java symbolic literals.")

;;; ................................................... &cpp-variables ...

(defvar my-lint-layout-cpp-variable-literals
  (concat
   "\\<"
   (regexp-opt
    '("false"
      "true"
      "NULL"))
   "\\>")
  "C++ symbolic literals.")

;;; ................................................... &php-variables ...

(defvar my-lint-layout-php-variable-literals
  (concat
   "\\<"
   (regexp-opt
    '("false"
      "true"
      "null"))
   "\\>")
  "PHP symbolic literals.")

(defconst my-lint-layout-php-function-call-keywords-no-paren
   (regexp-opt
    '("include"
      "include_once"
      "require"
      "require_once"
      "print"
      "echo")
    'words)
  "PHP functions that can be called without parens:

  print('this');
  print 'this';")

(defconst my-lint-layout-php-function-call-keywords-generic
  (concat
   "[^a-zA-Z0-9$_-]\\("
   (mapconcat
    'concat
    '(
      "array\\(?:_[a-z_]+\\)?"
      "date"
      "die"
      "empty"
      "ereg"
      "header"
      "is_[a-z]+"
      "isset"
      "md5"
      "mysql_[a-z_]+"
      "preg_[a-z]+"
      "strftime"
      "trim"
      )
    "\\|")
   "\\)\\>")
  "Typical PHP functions.")

(defconst my-lint-layout-php-function-regexp
  (concat
   "^\\([ \t]*\\)"  ;; subatch 1
   "\\(?:"
	    ;; <other modifier>?<access modifier>?function ()
	    my-lint-layout-generic-other-modifier-regexp
	    "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)?"

	    "\\|"

	    ;; <access modifier>?<other modifier>?function ()
	    "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)?"
	    my-lint-layout-generic-other-modifier-regexp
   "\\)?"
   "[ \t]*"
   ;; function name ()
   "\\<function\\>[ \t\r\n]+[^(]+(")
 "Function regexp. Submatch 1: Indent")

(defconst my-lint-layout-java-function-regexp
  (concat
   "^\\([ \t]*\\)"  ;; subatch 1
   "\\(?:"
	    ;; <other modifier>?<access modifier>?function ()
	    my-lint-layout-generic-other-modifier-regexp
	    "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)?"

	    "\\|"

	    ;; <access modifier>?<other modifier>?function ()
	    "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)?"
	    my-lint-layout-generic-other-modifier-regexp
   "\\)?"
   "[ \t]*"
   ;; name ()
   "[ \t\r\n]+[^(]+(")
 "Method regexp. Submatch 1: Indent")

(defconst my-lint-layout-php-variable-regexp
  (concat
   "^[ \t]*"
   "\\(?:"
       my-lint-layout-generic-access-modifier-regexp
       "[ \t]+"
       "\\|\\<var\\>[ \t]+"
       "\\)?"
   "\\$_*[a-zA-Z][ \t]*[;=]")
  "Class variable regexp.")

;; public [static] int <variable>
(defconst my-lint-layout-java-modifier-regexp
  (concat
   "^[ \t]+"  ;; Every variable in Java is intended
   "\\("
       my-lint-layout-generic-access-modifier-regexp
       "[ \t]+"
       "\\)"
   "\\("
       "\\(?:"
	   my-lint-layout-generic-other-modifier-regexp
	   "[ \t]+"
       "\\)?"
       my-lint-layout-generic-vartype-modifier-regexp
       "[ \t]+"
       "\\)")
  "Java access modified for methods and class variables.")

(defconst my-lint-layout-java-variable-regexp
  (concat
   my-lint-layout-java-modifier-regexp
   "[a-zA-Z0-9_$]+[ \t]*[;=]")
  "Class variable regexp.")

(defconst my-lint-layout-php-doc-location-regexp
   (concat
    "^\\(?:[ \t]*"
	 "\\|[<][?][a-z]*[ \t]*\\)"
    ;; Keywords
    "\\(" ;; static public ...
	"\\(?:"
		my-lint-layout-generic-other-modifier-regexp
		my-lint-layout-generic-access-modifier-regexp
		"\\)"
	"\\|\\<"
	(regexp-opt
	'("const"
	  "var"
	  "function"
	  "require"
	  "include")
	t)
	"\\>"
    "\\)")
  "Regexp to match location where PHPDoc blocks should exist.")

(defconst my-lint-layout-java-doc-location-regexp
   (concat
    ;; Keywords
    "\\(" ;; static public ...
	"\\(?:"
		my-lint-layout-generic-other-modifier-regexp
		my-lint-layout-generic-access-modifier-regexp
		"\\)"
	"\\|\\<"
	(regexp-opt
	'("final"
	  ;; "import"
	  )
	t)
	"\\>"
    "\\)")
  "Regexp to match location where javadoc blocks should exist.")

(defconst my-lint-layout-php-data-type-regexp
  (concat
   "\\<"
   (regexp-opt
    '("array"
      "boolean"
      "integer"
      "mixed"
      "object"
      "resource"
      "string")
    t)
   "\\>")
  "Valid datatypes.")

(defconst my-lint-layout-php-data-type-short-regexp
  (concat
   "\\<"
   (regexp-opt
    '("array"
      "bool"
      "int")
    t)
   "\\>")
  "Valid short datatypes.")

(defconst my-lint-layout-generic-doc-1st-line-ignore-regexp
  "constructor\\|destructor"
  "*Ignore first line wording check if regexp matches.")

(defconst my-lint-layout-generic-doc-line-regexp
  "^\\([ \t]*/[*][*]\\|[ \t]+[*]\\)"
  "Documentation comment line regexp.")

(defconst my-lint-layout-generic-brace-and-code-regexp
  "[}][ \t]*\r?\n[ \t]*\\([^{} \t\r\n]+\\)"
  "Match brace end } followed by immediate code.")

(defvar my-lint-layout-check-generic-functions
  '(my-lint-layout-check-whitespace
    my-lint-layout-check-line-length)
  "*List of generic lint functions.")

(defconst my-lint-layout-check-php-code-functions
  '(my-lint-layout-generic-class-count
    my-lint-layout-generic-xml-tags-check-main
    my-lint-layout-php-check-xml-tags-lazy
    my-lint-layout-php-check-multiple-print
    my-lint-layout-generic-check-statement-end
    my-lint-layout-generic-check-statement-start
    my-lint-layout-generic-check-statement-start-2
    my-lint-layout-generic-check-comment-statements
    my-lint-layout-generic-check-control-statements
    my-lint-layout-generic-check-block-end-and-code
    my-lint-layout-php-check-line-up-assignment
    my-lint-layout-generic-check-brace-extra-newline
    my-lint-layout-php-check-regexp-occur-main
    my-lint-layout-generic-class-check-variables
    my-lint-layout-php-check-input-form-main
    my-lint-layout-php-check-sql-kwd-statements
    my-lint-layout-php-check-multiline-print
    my-lint-layout-php-check-multiline-sql
    my-lint-layout-php-check-words
    my-lint-layout-php-check-keywords-main
    my-lint-layout-check-whitespace
    my-lint-layout-check-eof-marker
    my-lint-layout-check-line-length)
  "*List of PHP code check functions")

(defconst my-lint-layout-check-java-code-functions
  '(my-lint-layout-generic-class-count
    my-lint-layout-generic-check-statement-end
    my-lint-layout-generic-check-statement-start-brace-end
    my-lint-layout-generic-check-comment-statements
    my-lint-layout-generic-check-control-statements
    my-lint-layout-generic-check-block-end-and-code
    ;; my-lint-layout-php-check-line-up-assignment
    my-lint-layout-generic-check-brace-extra-newline
    my-lint-layout-java-check-regexp-occur-main
    my-lint-layout-generic-class-check-variables
    my-lint-layout-php-check-input-form-main
    ;; my-lint-layout-php-check-sql-kwd-statements
    ;; my-lint-layout-php-check-multiline-print
    ;; my-lint-layout-php-check-multiline-sql
    my-lint-layout-php-check-words
    ;; my-lint-layout-php-check-keywords-main
    ;; my-lint-layout-check-whitespace
    ;; my-lint-layout-check-eof-marker
    my-lint-layout-check-line-length)
  "*List of Java code check functions")

(defvar my-lint-layout-check-php-doc-functions
  '(my-lint-layout-php-check-doc-missing
    my-lint-layout-generic-check-doc-main)
  "*List of functions for PHPDoc.")

(defvar my-lint-layout-check-php-generic-functions
  (append my-lint-layout-check-php-doc-functions
	  my-lint-layout-check-php-code-functions)
  "List of all PHP check functions.")

(defvar my-lint-layout-check-java-doc-functions
  '(my-lint-layout-java-check-doc-missing
    my-lint-layout-generic-check-doc-main)
  "*List of functions for Javaoc.")

(defvar my-lint-layout-check-java-generic-functions
  (append my-lint-layout-check-java-doc-functions
	  my-lint-layout-check-java-code-functions)
  "List of all PHP check functions.")

(defvar my-lint-layout-check-sql-functions
  '(my-lint-layout-sql-check-statement-create-table-main
    my-lint-layout-sql-check-statement-select-main
    my-lint-layout-sql-check-statement-insert-into-main
;;;    my-lint-layout-sql-check-keywords
    my-lint-layout-sql-check-comments)
  "*List of functions for SQL.")

(defvar my-lint-layout-check-whitespace-functions
  '(my-lint-layout-whitespace-multiple-newlines
    my-lint-layout-whitespace-indent-space-only
    my-lint-layout-whitespace-indent-mixed
    my-lint-layout-whitespace-trailing
    my-lint-layout-whitespace-trailing-cr
    my-lint-layout-whitespace-at-eob)
  "*List of whitespace check functions.")

(defvar my-lint-layout-check-css-functions
  '(my-lint-layout-check-comment-javadoc-invalid
    my-lint-layout-css-check-generic
    my-lint-layout-css-comment-multiline-buffer
    my-lint-layout-css-check-regexp-occur-main)
  "*List of functions for CSS.")

(defconst my-lint-layout-css-web-safe-font-list
  '(;;  Courier is not defined in all Windows OSes by default
    "courier new"
    "arial"
    "georgia"
    ;; "Century Gothic" parhaps
    ;; "Trebuchet MS" perhaps
    ;; "times" not reliable anough
    "times new roman"
    "verdana")
  "*Web safe fonts compatible with all browsers and OSes.
http://en.wikipedia.org/wiki/Web-safe_fonts

Related articles:

    Tools: Reliability of web-safe fonts
    ... summarise cross-platform reliability for fonts that are
    ... commonly held to be safe for use in web-design
    http://www.webspaceworks.com/resources/fonts-web-typography/41/

    Fonts on the web and a list of web safe fonts
    http://dustinbrewer.com/fonts-on-the-web-and-a-list-of-web-safe-fonts")

(defconst my-lint-layout-css-web-safe-font-regexp
  (concat
   "\\<\\(?:"
   (regexp-opt
    my-lint-layout-css-web-safe-font-list)
   "\\)\\>")
  "*Regexp of `my-lint-layout-css-web-safe-font-list'.")

(defvar my-lint-layout-code-type nil
  "Variable set to a values of `my-lint-layout-code-type-p'.
Internal variable, not meant ot be set by use.

Set locally to a buffer in `my-lint-layout-code-type-set-local-variable'.
See also `my-lint-layout-check-file-list'.")

;;; ....................................................... &utilities ...

(put 'my-lint-layout-debug-message 'my-lint-layout-debug-message 0)
(put 'my-lint-layout-debug-message 'edebug-form-spec '(body))
(defmacro my-lint-layout-debug-message (&rest body)
  "Display debug @body using `message' function."
  `(when my-lint-layout-debug
     (message ,@body)))

(put 'my-lint-layout-with-result-buffer 'lisp-indent-function 0)
(put 'my-lint-layout-with-result-buffer 'edebug-form-spec '(body))
(defmacro my-lint-layout-with-result-buffer (&rest body)
  "Run body in `my-lint-layout-buffer-name'."
  `(let ((buffer (get-buffer-create my-lint-layout-buffer-name)))
     (with-current-buffer buffer
       ,@body)))

(defsubst my-lint-layout-code-java-search ()
  "Position point at first detected Java code line."
  (goto-char (point-min))
  (re-search-forward
   `,(concat
      "^[ \t]*import[ \t]+[a-z]+\\..*;"
      "\\|public.*static"
      "\\|\\<main[ \t]*("
      "\\|\\<\\(float\\|double\\|int\\|long\\|public\\)[ \t]+[^=;]+[=;]"
      "\\|@author\\|@since\\|@version")
   nil t))


(defsubst my-lint-layout-code-java-p (&optional filename)
  "Return non-nil if Java code. Optional check FILENAME."
  (or (memq major-mode '(java-mode))
      (and (stringp filename)
	   (string-match "\\.java" filename))
      (string-match "\\.java" (or (buffer-file-name) (buffer-name)))
      (save-excursion (my-lint-layout-code-java-search))))

(defsubst my-lint-layout-code-php-search ()
  "Position point at first detected PHP code line."
  (goto-char (point-min))
  ;; $variable
  (re-search-forward "^[ \t]*[$][a-zA-Z]" nil t))

(defsubst my-lint-layout-code-php-p (&optional filename)
  "Return non-nil if PHP code."
  (or (memq major-mode '(php-mode))
      (and (stringp filename)
	   (string-match "\\.php\\|\\.inc" filename))
      (string-match "\\.php\\|\\.inc" (or (buffer-file-name) (buffer-name)))
      (save-excursion (my-lint-layout-code-php-search))))

(defsubst my-lint-layout-code-css-p (&optional filename)
  "Return t if buffer is PHP code."
  (or (memq major-mode '(sql-mode))
      (and (stringp filename)
	   (string-match "\\.sql" filename))
      (string-match "\\.sql" (or (buffer-file-name) (buffer-name)))))

(defsubst my-lint-layout-code-sql-p (&optional filename)
  "Return t if buffer is PHP code."
  (or (memq major-mode '(css-mode))
      (string-match (buffer-name) "\\.css")))

(defsubst my-lint-layout-code-type-p (&optional filenam)
  "Return java-mode, php-mode, css-mode, sql-mode"
  (cond
   ((or (eq my-lint-layout-code-type 'java-mode)
	(my-lint-layout-code-java-p))
    'java-mode)
   ((or (eq my-lint-layout-code-type 'java-mode)
	(my-lint-layout-code-php-p))
    'php-mode)
   ((or (eq my-lint-layout-code-type 'java-mode)
	(my-lint-layout-code-sql-p))
    'sql-mode)
   ((or (eq my-lint-layout-code-type 'java-mode)
	(my-lint-layout-code-css-p))
    'css-mode)))

(defsubst my-lint-layout-code-type-set-local-variable ()
  "Set `my-lint-layout-code-type' local to buffer."
  (make-local-variable 'my-lint-layout-code-type)
  (setq my-lint-layout-code-type
	(my-lint-layout-code-type-p)))

(defsubst my-lint-layout-result-erase-buffer ()
  "Create and clear `my-lint-layout-buffer-name'."
  (my-lint-layout-with-result-buffer
    (setq buffer-read-only nil)
    (erase-buffer)))

(put 'my-lint-layout-with-case 'lisp-indent-function 0)
(put 'my-lint-layout-with-case 'edebug-form-spec '(body))
(defmacro my-lint-layout-with-case (&rest body)
  "Run BODY with `case-fold-search' set to nil."
  `(let (case-fold-search)
     ,@body))

(put 'my-lint-layout-with-point-min 'lisp-indent-function 0)
(put 'my-lint-layout-with-point-min 'edebug-form-spec '(body))
(defmacro my-lint-layout-with-point-min (&rest body)
  "Run BODY with from `point-min'. Point is preserved."
  `(save-excursion
    (goto-char (point-min))
    ,@body))

(put 'my-lint-layout-with-save-point 'lisp-indent-function 0)
(put 'my-lint-layout-with-save-point 'edebug-form-spec '(body))
(defmacro my-lint-layout-with-save-point (&rest body)
  "Run body and restore point. Lighter than `save-excursion'."
  (let ((point (gensym "point-")))
    `(let ((,point (point)))
       (prog1
	   (progn
	     ,@body)
	 (goto-char ,point)))))

(put 'my-lint-layout-flet-run-at-point 'lisp-indent-function 0)
(put 'my-lint-layout-flet-run-at-point 'edebug-form-spec '(body))
(defmacro my-lint-layout-flet-run-at-point (&rest body)
  "Define function `run' which preserves point. Run BODY."
  (let ((point (make-symbol "--point--")))
    `(let ((,point (point)))
       (flet ((run (func &rest args)
		   (goto-char ,point)
		   (apply func args)))
	 ,@body))))

(put 'my-lint-with-result-buffer 'lisp-indent-function 2)
(put 'my-lint-with-result-buffer 'edebug-form-spec '(body))
(defmacro my-lint-with-result-buffer (display erase &rest body)
  "Clear result buffer, run BODY, collect results and DSPLAY."
  `(progn
     (if ,erase
	 (my-lint-layout-result-erase-buffer))
     ,@body
     (my-lint-layout-result-sort-lines)
     (when ,display
       (display-buffer my-lint-layout-buffer-name)
       (with-current-buffer my-lint-layout-buffer-name
	 (my-lint-output-mode)))))

(put 'my-lint-layout-with-interactive 'lisp-indent-function 0)
(put 'my-lint-layout-with-interactive 'edebug-form-spec '(body))
(defmacro my-lint-layout-with-interactive (&rest body)
  "Run BODY from `point-min' and show `my-lint-layout-buffer-name'."
  `(progn
     (my-lint-layout-result-erase-buffer)
     (save-excursion
       (goto-char (point-min))
       ,@body)
     (with-current-buffer my-lint-layout-buffer-name
       (sort-lines nil (point-min) (point-max)))
     (display-buffer my-lint-layout-buffer-name)))

(defsubst my-lint-layout-prefix (prefix)
  "If PREFIX, add it as filename to the beginning."
  (if prefix
      (format "%s:" prefix)
    ""))

(defsubst my-lint-layout-current-line-string ()
  "Current line."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defsubst my-lint-layout-expand-tabs (str)
  "Expand tabs to spaces."
  (replace-regexp-in-string "\t" `,(make-string 8 ?\ ) str))

(defsubst my-lint-layout-paragraph-end-point ()
  "Return empty line or `point-max'."
  (save-excursion
    (or (re-search-forward "^[ \t]*$" nil t)
	(point-max))))

(defun my-lint-layout-count-char-in-string (c s)
  "Count character C in string S ."
  (let ((count 0)
	(pos   0))
    (while (< pos (length s))
      (if (char-equal (aref s pos) c)
	  (incf  count))
      (incf  pos))
    count))

(defun my-lint-layout-current-line-number-traditional ()
  "Return line number. Lines are counted from 1..x"
  ;;  - always use line beginning as reference
  ;;  - The count-lines returns 0 for 1st line, therefore 1+
  (let ((beg (line-beginning-position))
	(i 1)
	(point (point)))
    (goto-char (point-min))
    (while (search-forward "\n" beg t)
      (incf i))
    (goto-char point)
    i))

(defun my-lint-layout-current-line-number (&optional point)
  "Return line number."
  (if (fboundp 'line-number-at-pos)
      (line-number-at-pos point)
    (my-lint-layout-current-line-number-traditional)))

(defsubst my-lint-layout-buffer-data-p ()
  "Check that buffer contains text."
  (string-match "[^ \t\r\n]" (buffer-string)))

(defsubst my-lint-layout-count-lines-in-string (str)
  "Count lines in STR."
  (length (replace-regexp-in-string "[^\n]" "" str)))

(defsubst my-lint-layout-searc-backward-brace-open ()
  "Move to opening brace backward."
  (re-search-backward "^[ \t]*{" nil t))

(defsubst my-lint-layout-top-level-p ()
  "Return t if outside of brace blocks. Point is moved."
  (not (my-lint-layout-searc-backward-brace-open)))

(defsubst my-lint-layout-doc-package-string-p (str)
  "Check top level @tokens"
  (string-match "@package\\|@copyright\\|@author\\|@version" str))

(defsubst my-lint-layout-doc-var-string-p (str)
  "Check @package phpdoc"
  (string-match "@var" str))

(defsubst my-lint-layout-looking-at-doc-p ()
  (and (looking-at my-lint-layout-generic-doc-line-regexp)
       (match-beginning 0)))

(defsubst my-lint-layout-search-backward-doc-beginning ()
  (and (re-search-backward "^[ \t]*/[*][*]" nil t)
       (match-beginning 0)))

(defsubst my-lint-layout-search-forward-doc-beginning ()
  (and (re-search-forward "^[ \t]*/[*][*]" nil t)
       (match-beginning 0)))

(defsubst my-lint-layout-search-forward-doc-end ()
  (and (re-search-forward "[*]/" nil t)
       (match-end 0)))

(defsubst my-lint-layout-generic-search-forward-function-beginning ()
  (cond
   ((eq 'php-mode (my-lint-layout-code-type-p))
    (re-search-forward my-lint-layout-php-function-regexp nil t))
   ((eq 'java-mode (my-lint-layout-code-type-p))
    (re-search-forward my-lint-layout-java-function-regexp nil t))
   (t
    (error "Unkown file type for buffer %s" (buffer-name)))))

(defsubst my-lint-layout-search-forward-class-beginning (&optional max)
  "Search class forward, up till optional MAX point."
  (and (re-search-forward
	"^[ \t]*\\(?:\\(abstract\\)[ \t]*\\)?class\\>[ \t]"
	max t)
       (match-end 0)))

(defsubst my-lint-layout-search-backward-class-beginning (&optional max)
  "Search class backward, up till optional MAX point."
  (and (re-search-backward
	"^[ \t]*\\(?:\\(abstract\\)[ \t]*\\)?class\\>[ \t]"
	max t)
       (match-end 0)))

(defsubst my-lint-layout-search-forward-interface-beginning (&optional max)
  "Search interface forward, up till optional MAX point."
  (and (re-search-forward
	"^[ \t]*interface\\>[ \t]"
	max t)
       (match-end 0)))

(defsubst my-lint-layout-search-backward-interface-beginning (&optional max)
  "Search interface backward, up till optional MAX point."
  (and (re-search-backward
	"^[ \t]*interface\\>[ \t]"
	max t)
       (match-end 0)))

(defsubst my-lint-layout-search-forward-class-p ()
  "See if class or interface exists."
  (or (my-lint-layout-search-forward-class-beginning)
      (my-lint-layout-search-forward-interface-beginning)))

(defsubst my-lint-layout-search-backward-class-p ()
  "See if class or interface exists."
  (or (my-lint-layout-search-backward-class-beginning)
      (my-lint-layout-search-backward-interface-beginning)))

(defsubst my-lint-layout-search-forward-ending-semicolon ()
  "Search ending semicolon.
Note: the (match-beginning 0) is the semicolon,
and  (match-string 1) contains possible comment start.
The comment marker, if any, is in (match-string 2)."
  (re-search-forward ";[ \t]*\\(\\(//\\|#\\|/\\*\\).*\\)?$" nil t))

(defsubst my-lint-layout-search-forward-variable-beginning (&optional max)
  (re-search-forward
   (cond
    ((eq 'php-mode (my-lint-layout-code-type-p))
     my-lint-layout-php-variable-regexp)
    ((eq 'java-mode (my-lint-layout-code-type-p))
     my-lint-layout-java-variable-regexp)
    (t
     (error "Error: Unknown file type for buffer %s" (buffer-name))))
    max t))

(defsubst my-lint-layout-type-statement-string-p (str)
  "Test if statement contains semicolon at the end."
  (string-match ";[ \t]*$" str))

;; For example: "private $var;"
(defsubst my-lint-layout-type-class-variable-dollar-string-p (str)
  (string-match
   `,(concat
      ".*"
      my-lint-layout-generic-access-modifier-regexp
      ".*[$].*;[ \t]*$")
   str))

(defsubst my-lint-layout-generic-function-regexp ()
  (cond
   ((eq 'php-mode (my-lint-layout-code-type-p))
    my-lint-layout-php-function-regexp)
   ((eq 'java-mode (my-lint-layout-code-type-p))
    my-lint-layout-java-function-regexp)))

(defsubst my-lint-layout-type-function-string-p (str)
  (string-match
   (my-lint-layout-generic-function-regexp)
   str))

(defsubst my-lint-layout-type-include-string-p (str)
  "Check require, include"
  (string-match "^[ \t]*\\(require\\|include\\)" str))

(defsubst my-lint-layout-type-import-string-p (str)
  "Check require, include"
  (string-match "^[ \t]*\\(import\\)" str))

(defsubst my-lint-layout-type-class-string-p (str)
  (string-match "^[ \t]*class" str))

;; For example: "$var = ...;"
(defsubst my-lint-layout-type-variable-string-p (str)
  (string-match
   `,(concat
      "^[ \t]*"
      "\\(?:"
      my-lint-layout-generic-access-modifier-regexp
      "\\)?"
      "[ \t]*[$][a-z].*[ \t];"  ;; $var = value;
      "[ \t]*$")
   str))

(defsubst my-lint-layout-looking-at-doc-end-valid-p ()
  "Check that */ is followed by a function or variable definition."
  (and
   (looking-at
    `,(concat
       ;; Can't use regexp-opt, it uses grouping parens.
       "[ \t]*\\<\\("
       "class"
       "\\|function"
       "\\|public"
       "\\|protected"
       "\\|private"
       "\\|static"
       "\\|var"
       "\\|require"
       "\\|include"
       "\\)\\>"))
   (match-string 1)))

(defsubst my-lint-layout-string-mixed-case-p (str)
  "Check if STR contaisn MixedCase characters."
  (my-lint-layout-with-case
    (string-match "[a-z][A-Z]\\|[A-Z][a-z]" str)))

(defsubst my-lint-layout-string-iso-date-p (str)
  "Match YYYY-MM-DD.

The submatches are as follows. Possible HH:MM:SS is included in (2).
    YYYY-MM-DD
   122222222223
   |          |character after, if any
   Character before, if any"
  (string-match
   `,(concat
      "\\(.\\)"
      "\\("
      "[0-9]\\{4,4\\}-[0-9][0-9]-[0-9][0-9]" ;; YYYY-MM-DD
      ;; HH:MM:SS
      "\\(?:[ \t]+[0-9][0-9]\\(?::[0-9][0-9]\\)\\{1,2\\}?\\)?"
      "\\)"
      "\\(.?\\)"))
   str)

(defsubst my-lint-layout-string-uppercase-p (str)
  "Check if STR is all uppercase."
  (my-lint-layout-with-case
    (string-match "^[A-Z:_ \t\r\n]+$" str)))

(defsubst my-lint-layout-string-lowercase-p (str)
  "Check if STR is all lowercase."
  (my-lint-layout-with-case
    (string-match "^[a-z:_ \t\r\n]+$" str)))

(defsubst my-lint-layout-string-comment-p (str)
  "Check if STR looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\|#\\)" str))

(defsubst my-lint-layout-looking-at-assignment-column-p ()
  "Return assignmnet '=' column position."
  ;;  Do not count equal '=='
  (when (looking-at
	 `,(concat
	   "^[ \t]*"
	   my-lint-layout-generic-access-modifier-regexp
	   "?[ \t]*"
	   "[$a-z0-9_->]+[^=\r\n]*=[^=]"
	   ))
    (- (length (match-string 0)) 2)))

(defsubst  my-lint-layout-make-result-header-string ()
  "Return file and time string."
  (format "== %s %s\n"
	  (if buffer-file-name
	      buffer-file-name
	    (buffer-name))
	  (format-time-string "%Y-%m-%d %H:%M")))

(defsubst my-lint-layout-looking-at-empty-line-p ()
  "If `looking-at' at empty line."
  (looking-at "[ \t]*$"))

(defsubst my-lint-layout-looking-at-comment-doc-block-p ()
  "If `looking-at' at /** doc block."
  (looking-at "[ \t]*/\\*\\*[ \t\r\n]"))

(defsubst my-lint-layout-looking-at-comment-start-any-p ()
  "If `looking-at' at comment start."
  (looking-at "^[ \t]*\\(/?[*]\\|//\\)"))

(defsubst my-lint-layout-looking-at-comment-start-multiline-p ()
  "If `looking-at' at comment start."
  (looking-at "^[ \t]*/[*]"))

(defsubst my-lint-layout-comment-skip-multiline ()
  "Skip to multiline comment end."
  (re-search-forward "\\*/[ \t\r]*$" nil t))

(defsubst my-lint-layout-looking-at-comment-start-single-p ()
  "If `looking-at' at comment start."
  (looking-at "^[ \t]*\\(//\\|[#]\\)"))

(defsubst my-lint-layout-looking-at-comment-end-p ()
  "If `looking-at' at comment end."
  (looking-at "^[ \t]*\\(//\\|[*]/\\)"))

;; FIXME: use string-match
(defsubst my-lint-layout-looking-at-comment-point-p ()
  "If `looking-at' at comment."
  (my-lint-layout-with-save-point
    (goto-char (line-beginning-position))
    (looking-at "^[ \t]*\\(/?[*]\\|//\\|[*]/\\)")))

(defsubst my-lint-layout-looking-at-comment-line-p ()
  "Check if line looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\)"
		(my-lint-layout-current-line-string)))

(defsubst my-lint-layout-looking-at-statement-p ()
  "If `looking-at' at semicolon at the end of line."
  (my-lint-layout-with-save-point
    (goto-char (line-beginning-position))
    (looking-at "^.*;[ \t\r]*\n")))

(defsubst my-lint-layout-looking-at-variable-at-line-p ()
  "If `looking-at' at variable at line"
  (my-lint-layout-with-save-point
    (goto-char (line-beginning-position))
    (re-search-forward "[$]_*[a-z0.9]+" (line-end-position) t)))

(defsubst my-lint-layout-looking-at-conditional-p ()
  "If `looking-at' conditional statement."
  (looking-at (concat
	       "^[ \t]*"
	       my-lint-layout-generic-control-statement-start-regexp)))

(defsubst my-lint-layout-looking-at-continue-statement-p ()
  "If `looking-at' continued statement, like 'else'."
  (looking-at (concat
	       "^[ \t]*"
	       my-lint-layout-generic-control-statement-continue-regexp)))

(defsubst my-lint-layout-looking-at-control-statement-p ()
  "if..else."
  (or (my-lint-layout-looking-at-conditional-p)
      (my-lint-layout-looking-at-continue-statement-p)))

;; FIXME
(defsubst my-lint-layout-code-statement-end-search ()
  "Search until statement end semicolon."
  (let ((search t))
    (while (and (not (eobp))
		search)
      (forward-line 1)
      (cond
       ((my-lint-layout-looking-at-comment-start-multiline-p)
	(my-lint-layout-comment-skip-multiline))
       ((my-lint-layout-looking-at-comment-start-single-p))
       ((looking-at "^.*;[ \t\r]*$")
	(setq search nil))))))

(defsubst my-lint-layout-result-header-string-insert ()
  "Insert file and time string."
  (insert (my-lint-layout-php-result-buffer-header)))

(defsubst my-lint-layout-result-sort-lines ()
  "Sort lines."
  (my-lint-layout-with-result-buffer
    (sort-lines (not 'reverse) (point-min) (point-max))))

(defun my-lint-layout-message (msg &optional prefix line)
  "Write MSG with LINE numnber using PREFIX.
See `my-lint-layout-buffer-name'."
  (or line
      (setq line (my-lint-layout-current-line-number)))
  (my-lint-layout-with-result-buffer
    (goto-char (point-max))
    (insert (format "%s%04d: %s\n"
		    (my-lint-layout-prefix prefix)
		    line
		    msg))
    t))

(defsubst my-lint-layout-doc-line-startp-p ()
  "Check /* line. Return starting point."
  (when (looking-at "^\\([ \t]*\\)/[*]")
    (match-end 1)))

(defsubst my-lint-layout-doc-line-end-p ()
  "Check /* line. Return starting point."
  (when (looking-at "^\\([ \t]+\\)[*]/")
    (match-end 1)))

(defsubst my-lint-layout-doc-line-indent-p ()
  "Return indent of documentation comment.

/*
 *  This test starts indent which is 2 towards the star.
 */
>>>
122

The leading indent is in submatch 1 and text start indent in 2."
  (if (looking-at "^\\([ \t]*\\)[*]\\([ \t]*\\)")
      (match-end 1)))

(defsubst my-lint-layout-generic-comment-multiline-p ()
  "Check '*' character to the left. Point must be at the beginning of line."
  (looking-at "^[ \t]*\\*"))

(defun my-lint-layout-generic-comment-multiline-forward ()
  "Search multiline comment. Return comment region points '(beg end).
/*

 */"
  (when (re-search-forward "^[ \t]*/\\*" nil t)
    (let ((beg (match-beginning 0)))
      (when (re-search-forward "^[ \t]*\\*/" nil t)
	(list beg (match-end 0))))))

(defun my-lint-layout-generic-comment-multiline-stars ()
  "Check if multiline comment contains stars or not.
/*
 * Stars to the left
 *
 */

Return:

  '(BEG END POINT)

  If there are problems return comment region BEG END and
  POINT of problematic line."
  (multiple-value-bind (beg end)
      (my-lint-layout-generic-comment-multiline-forward)
    (when beg
      (save-excursion
	(goto-char beg)
	(forward-line 1)
	(while (and (< (point) end)
		    (my-lint-layout-generic-comment-multiline-p)
		    (forward-line 1)))
	(if (< (point) end)
	    (list beg end (point)))))))

(defun my-lint-layout-generic-check-comment-multiline-stars
  (beg end &optional prefix)
  "Check multiline comment in region BEG END."
  (let (col
	col-found)
    (save-excursion
      (goto-char beg)
      (search-forward "*")
      (setq col (1- (current-column)))
      (forward-line 1)
      (while (and (< (point) end))
	(cond
	 ((not (my-lint-layout-generic-comment-multiline-p))
	  (my-lint-layout-message
	   "[comment] multiline comment is missing a star(*) to the left"
	   prefix))
	 (t
	  (goto-char (1- (match-end 0)))
	  (setq col-found (current-column))
	  (unless (eq col col-found)
	    (my-lint-layout-message
	     (format
	      `,(concat "[comment] star(*) at col %d possibly"
			"not lined up with start col %d")
	      col-found col)
	     prefix))))
	(forward-line 1)))))

(defun my-lint-layout-generic-run-list (list &optional prefix point)
  "Run LIST of functions from current point forward.
Point is preserved after each function. Result buffer is not
displayed."
  (dolist (function list)
    (my-lint-layout-with-save-point
      (if point
	  (goto-char point))
      (my-lint-layout-debug-message
       "debug layout: check %s %s %s" function prefix (buffer-name))
      (funcall function prefix))))

;;; ........................................................... &occur ...

(defconst my-lint-layout-generic-check-regexp-occur-line-up-style-list
  (list
   ;; Starting brace
   '("[a-z].*{[ \t\r\n]*$"
     "possibly K&R brace style, expect line-up")

   ;; Ending brace
   '("[a-z].*}[ \t\r\n]*$"
     "possibly K&R brace style, expect line-up"))
  "*K&R Brace placement checks.")

(defconst my-lint-layout-php-check-regexp-occur-global-uppercase
  (list
   '("^[ \t]*global[ \t]+\\([$].*[a-z].*\\);"
     "global variable name not all uppercase"
     nil
     t))
  "*CamelCase variable checks.")

(defconst my-lint-layout-php-check-regexp-occur-camelcase-style-list
  '("^[ \t]*[$]?[a-z0-9]+_[a-z0-9_]+[ \t\r\n]*="
    "variable name not camelCase"
    nil
    nil
    (lambda ()
      (let ((str (my-lint-layout-current-line-string)))
	(my-lint-layout-with-case
	  ;; Global variable
	  (not (string-match "[$]?[A-Z][A-Z0-9]+_" str))))))
  "*CamelCase variable checks.")

(defconst my-lint-layout-java-check-regexp-occur-camelcase-style-list
  (list
   (list
    `,(concat
       my-lint-layout-java-modifier-regexp
       "[a-zA-Z0-9]+_[a-zA-Z0-9_]+[ \t\r\n]*[=;]")
    "variable name not camelCase"
    nil
    nil
    (lambda ()
      (let ((str (my-lint-layout-current-line-string)))
	(my-lint-layout-with-case
	  ;; Global variable
	  (not (string-match "[$]?[A-Z][A-Z0-9]+_" str)))))))
  "*CamelCase variable checks.")

(defun my-lint-layout-generic-run-occur-list (list &optional prefix)
  "Check LIST of regexps."
  (let (line point)
    (dolist (elt list)
      (multiple-value-bind (re msg not-re case func) elt
	(save-excursion
	  (let ((case-fold-search (if case
				      nil
				    t)))
	    (while (re-search-forward re nil t)
	      (setq point (point))
	      (goto-char (line-beginning-position))
	      (cond
	       ((my-lint-layout-looking-at-comment-start-single-p)
		(goto-char point))
	       ((my-lint-layout-looking-at-comment-start-multiline-p)
		(my-lint-layout-comment-skip-multiline))
	       (t
		(goto-char point)
		(setq line (my-lint-layout-current-line-string))
		(when (and
		       (or (null not-re)
			   (not (or (save-match-data
				      (string-match not-re line))
				    (save-excursion
				      (goto-char (line-beginning-position))
				      (looking-at not-re)))))
		       (or (null func)
			   (funcall func)))
		  (my-lint-layout-message
		   (format "[code] %s: %s"
			   msg
			   (my-lint-layout-current-line-string))
		   prefix)))))))))))

(defun my-lint-layout-generic-run-occur-variable-list (list &optional prefix)
  "Check LIST of varibales that contain regexps."
  (dolist (var list)
    (save-excursion
      (my-lint-layout-generic-run-occur-list
       (symbol-value var) prefix))))

;;; ...................................................... &occur-java ...

(defconst my-lint-layout-java-check-regexp-occur-funcdef-style-list
  (list
   ;; function name ()
   (list
    (concat
     "^[ \t]+"
     "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)[ \t]+"
     "[^(][ \t]+("
     "in method definition, space before starting paren"))

   ;; function this_is_name()
   (list
    (concat
     "^[ \t]+"
     "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)[ \t]+"
     "[A-Z][^(]*[ \t]*("
     "in method definition, name not camelCase"))

   ;; function name( param, def)
   (list
    (concat
     "^[ \t]+"
     "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)[ \t]+"
     "[A-Z][^(]*[ \t]*([\t ]"
     "in method definition, extra space after opening paren"))

   ;; function name(param, def )
   (list
    (concat
     "^[ \t]+"
     "\\(?:" my-lint-layout-generic-access-modifier-regexp "\\)[ \t]+"
     "[A-Z][^(]*[ \t]*([^)\r\n]*[ \t])"
     "in method definition, extra space before closing paren")))
  "Check Modern layout style.")

(defconst my-lint-layout-java-check-regexp-occur-list
  (list

   '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)("
     "in statement, no space between keyword and starting paren")

   '("[a-z0-9]\\([&][&]\\|[|][|]\\|[><]=?\\|[!=]=\\)"
     "in statement, no space before operator"
     ;; <address@example.com>
     ;; /**
     ;;  * comment
     ;;  */
     "@\\|^[ \t]/?*\\*")

   '("\\([&][&]\\|[|][|]\\|[><]=?\\|[!=]=\\)[a-z0-9]"
     "in statement, no space after operator"
     ;; <address@example.com>
     ;; /**
     ;;  * comment
     ;;  */
     "@\\|^[ \t]/?*\\*")

   ;; '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)[ \t]*([^ \t\r\n]"
   ;;   "in statement, no space after starting paren")

   ;; '("\\<\\(if\\|else\\|foreach\\|for\\|while\\)[ \t]*([^ $)\t\r\n]"
   ;;   "in statement, no space after keyword and paren")

   ;; funcall(arg )
   '("\\<[_a-zA-Z][._a-zA-Z0-9]+([^)\r\n]*[ \t]+)"
     "in method call, possibly extra space before closing paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)\\|^[ \t]/?*\\*")

   ;; funcall( arg)
   '("\\<[_a-zA-Z][._a-zA-Z0-9>-]+([ \t]+[^)\r\n]*)"
     "in method call, possibly extra space after opening paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)\\|^[ \t]/?*\\*")

   ;; funcall (arg)
   '("^[ \t]+\\<[_a-zA-Z][._a-zA-Z0-9]+[ \t]+([^);\r\n]*)"
     "in method call, possibly extra space before opening paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)\\|^[ \t]/?*\\*")

   ;; this.funcall (arg)
   '("this\\.[^][ )\t\r\n]+[ \t]+("
     "in method call, possibly extra space before opening paren")

   ;; funcall(arg,arg)
   '("[ \t]+\\<[_a-zA-Z][._a-zA-Z0-9]+[ \t]*([^;)\r\n]+,[^ ,;)\r\n]+)"
     "in method call, no space after comma"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; code );
   '("[^) \t\r\n]+[ \t]+);"
     "in method call, possibly extra space before closing paren (statement)"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   '("[a-zA-Z][a-zA-Z0-9_]*=[ \t]+[a-zA-Z0-9_\"\']"
     "in var assign, no space at left of equal sign")

   '("^[ \t]*}[ \t]*[\r\n][ \t]*\\<else\\>"
     "in block, 'else' not at previous brace line '}'")

   '("^[ \t]*}\\(else\\|catch\\|finally\\)\\>"
     "in block, no space after brace '}'")

   '("[a-zA-Z][a-zA-Z0-9_]*[ \t]+=[a-zA-Z0-9_\"'<]"
     "in var assign, no space at right of equal sign"))
  "Search ((REGEXP MESSAGE [NOT-REGEXP] [CASE-SENSITIVE] [FUNC]) ..).
See `my-lint-layout-generic-run-occur-list'.")

(defvar my-lint-layout-java-check-regexp-occur-variable
  '(my-lint-layout-java-check-regexp-occur-funcdef-style-list
    my-lint-layout-java-check-regexp-occur-list
    my-lint-layout-java-check-regexp-occur-camelcase-style-list)
  "*List of occur variable names.")

(defun my-lint-layout-java-check-regexp-occur-main (&optional prefix)
  "Run all occur checks."
  (my-lint-layout-generic-run-occur-variable-list
   my-lint-layout-java-check-regexp-occur-variable prefix))

(defun my-lint-layout-java-check-regexp-occur-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-java-check-regexp-occur-main'."
  (my-lint-layout-with-point-min
    (my-lint-layout-java-check-regexp-occur-main prefix)))

(defun my-lint-layout-java-check-regexp-occur-buffer-interactive ()
  "Call `my-lint-layout-java-check-regexp-occur-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-java-check-regexp-occur-buffer)))

;;; ....................................................... &occur-php ...

(defconst my-lint-layout-php-check-regexp-occur-modern-style-list
  (list
   '("^[ \t]*function[ \t]+[a-z0-9_]+("
     "in funcdef, no space before starting paren")

   '("^[ \t]*function[ \t][a-z0-9]+_[^ \t]*[ \t]*("
     "in funcdef, name not camelCase"))
  "Check Modern layout style.")

;; NOTES:
;; *) It's okay to use "@" suppression
;;
;;  $this->conn = @mysql_connect(DBHOST, DBUSER, DBPASS);
;;  if ( ! $this->conn )
;;
;;  (my-lint-layout-generic-run-occur-list my-lint-layout-php-check-regexp-occur-list)
(defconst my-lint-layout-php-check-regexp-occur-list
  (list

   ;; See PHP manual Security => Using Register Globals
   (list
    (concat
     "\\<[$]\\(HTTP\\)_[A-Z]+_[A-Z][A-Z_]+\\>")
    "security risk. Superglobal variables may be supported in newest PHP")

   '("\\<register_globals[ \t\r\n]+="
     "Security risk. Function register_globals() is deprecated.")

   (list
    (concat
     "^[ \t]*"
     "\\<\\(function\\|if\\|else\\(?:[ \t\r\n]*if\\)?"
	   "\\|while\\|class\\|abstract\\|interface\\|foreach\\)"
     "[ \t]*("
     "\\>")
    "Possibly misspelled keyword, expect lowercase"
    nil
    nil
    '(lambda ()
       (let* ((str   (match-string 0))
	      (lower (downcase str)))
	 (not (string= str lower)))))

   '("[|][|]\\|&&"
     "Readable 'and|or' alternative suggested for relational op")

   '("$_[a-z]+"
     "leading underscore in variable name"
     ;; http://fi2.php.net/manual/en/language.variables.superglobals.php
     "GLOBALS\\|SERVER\\|GET\\|POST\\|SESSION\\|FILES\\|REQUEST\\|COOKIE\\|ENV")

   '("^[ \t]*#"
     "Unknown commenting style"
     ;;  Skip CSS tags inside PHP:
     ;;
     ;;  print <<<CSS
     ;;  #id { ...
     ;;  CSS
     ".*[ \t\r\n]*{")

   '("^[ \t]*var[ \t]+[a-z].*;"
     "old vardef, migrate to syntax public|protected: ")

   ;; "$var" . "string"
   '("\"[$]_*[a-zA-Z0-9]+\""
     "double quotes around simple variable not needed")

   '("\\<ereg[_a-z]*(.*)"
     "preg*() function family recommended for")

   '("\\<include[_a-z][( \t]*[\"\'$]"
     "require*() function family is safer than")

   '("\\<echo[( \t]*[\"\'$]"
     "standard print() recommended for")

   '("\\<isset[( \t]*[\"\'$]"
     "possible better alternative is to use empty() test")

   '("[.=][ \t]*\\<date(.*)"
     "POSIX standard strftime() recommended for")

   '("\\<\\(if\\|else\\|elseif\\)[ \t]*(.*[$a-z][ \t]*=[ \t]*[$a-z]"
     "assignment inside statement"
     "mysql")

   '("\\$[a-z].*=[^;\n]*\n[^$\r\n]*<<<"
     "in assignment, HERE doc start not right of (=)")

   '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)("
     "in statement, no space between keyword like 'if' and starting paren")

   '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)[ \t]*([^ \t\r\n]"
     "in statement, no space after starting paren")

   '("\\<\\(if\\|else\\|foreach\\|for\\|while\\)[ \t]*([^ $)\t\r\n]"
     "in statement, no space after keyword and paren")

   ;; Multiline statement not handled: See >><<
   ;;
   ;;    if ( empty($a>>)
   ;;         and
   ;;         $b )
   '("\\<\\(if\\|foreach\\|while\\)[ \t]*(.*[^ \t\r\n])[ \t]*\r?\n.*{"
     "in statement, possibly no space before closing paren")

   '("this->[^][ )\t\r\n]+[ \t]+("
     "in funcall, possibly extra space before opening paren")

   ;; this ->var;
   '("\\<$?this[ \t]+->[ \t]*[_a-zA-Z]"
     "in ref, leading space before token ->")

   ;; this ->var;
   '("\\<$?this->[ \t]+[_a-zA-Z]"
     "in ref, leading space after token ->")

   ;; funcall(code )
   '("\\<[_a-zA-Z][_a-zA-Z0-9>-]+([^)\r\n]*[ \t]+)"
     "in funcall, possibly extra space before closing paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; funcall( code)
   '("\\<[_a-zA-Z][_a-zA-Z0-9>-]+([ \t]+[^)\r\n]*)"
     "in funcall, possibly extra space after opening paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; code );
   '("[^) \t\r\n]+[ \t]+);"
     "in funcall, possibly extra space before closing paren (statement)"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; function ( param, def)
   (list
    (concat
     "^[ \t\r\n]*"
     my-lint-layout-generic-access-modifier-regexp "?[ \t\r\n]*"
     "function[ \t]+"
     ".*([ \t]")
     "in funcdef, extra space after opening paren")

   ;; function (param, def )
   (list
    (concat
     "^[ \t\r\n]*"
     my-lint-layout-generic-access-modifier-regexp "?[ \t\r\n]*"
     "function[ \t]+"
     ".*([^)\r\n]*[ \t])")
    "in funcdef, extra space before closing paren")

   ;; FIXME $str = "string: $class.$method";
   ;;
   ;; (list
   ;;  "[$][a-z0-9_>-]+[.][$\"'][^);]"
   ;;  "no surrounding spaces, case 1, around concat(.)")

   (list
    "[\"'][.][$][a-z_]"
    "no surrounding spaces, case 2, around concat(.)")

   (list
    "[)][.][\"][^);]"
    "no surrounding spaces, case 3, around concat(.)")

   ;; if ( $query and mysql_result == false )
   (list
    (concat
     "\\<\\(?:else[ \t]*if\\|if\\|foreach\\|while\\)[ \t]*("
     ".*[ \t][$][^ 0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|and\\|or\\)[ \t]+"
     "[a-z0-9_]+[) \t\r\n]")
    "possibly missing vardef($) in relational test at right")

   ;; if ( value and $var )
   (list
    (concat
     "\\<\\(?:elseif\\|if\\|foreach\\|while\\)[ \t]*("
     "[ \t][^ $0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|\\<and\\>\\|\\<or\\>\\)[ \t]*"
     "[$][a-z0-9_]+[) \t\r\n]")
    "possibly missing vardef($) in relational test at left")

   '("[$][a-z][_a-z0-9]*=[ \t]+[$a-z_0-9\"\'<]"
     "no space at left of equal sign")

   '("[$][a-z][_a-z0-9]*[ \t]+=[$a-z_0-9\"'<]"
     "no space at right of equal sign")

   '("[$][a-z][_a-z0-9]*=[$a-z_0-9\"'<]"
     "no spaces around equal sign")

   '("[!=]=[ \t]*\\(null\\|true\\|false\\)"
     "possibly unnecessary test against literal"
     nil
     nil
     (lambda ()
       (not
	;;  Skip PHPUnit tests
	(string-match "assert"
		      (buffer-substring (max (point-min)
					     (- (point) (* 4 80)))
					(line-end-position))))))

   '("\\<function[ \t]+\\(de\\|con\\)struct"
     "possibly mispelled __(de|con)struct"))
  "Search ((REGEXP MESSAGE [NOT-REGEXP] [CASE-SENSITIVE] [FUNC]) ..).

See `my-lint-layout-generic-run-occur-list'.")

(defvar my-lint-layout-php-check-regexp-occur-variable
  '(my-lint-layout-php-check-regexp-occur-modern-style-list
    my-lint-layout-php-check-regexp-occur-list
    my-lint-layout-php-check-regexp-occur-camelcase-style-list
    my-lint-layout-php-check-regexp-occur-global-uppercase
    my-lint-layout-generic-check-regexp-occur-line-up-style-list)
  "*List of occur variable names.")

(defun my-lint-layout-php-check-regexp-occur-main (&optional prefix)
  "Run all occur checks."
  (my-lint-layout-generic-run-occur-variable-list
   my-lint-layout-php-check-regexp-occur-variable prefix))

(defun my-lint-layout-php-check-regexp-occur-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-php-check-regexp-occur-main'."
  (my-lint-layout-with-point-min
    (my-lint-layout-php-check-regexp-occur-main prefix)))

(defun my-lint-layout-php-check-regexp-occur-buffer-interactive ()
  "Call `my-lint-layout-php-check-regexp-occur-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-php-check-regexp-occur-buffer)))

;;; ............................................................ &misc ...

(defun my-lint-layout-generic-class-forward ()
  "Goto next class definition."
  (let ((class  (save-excursion
		  (my-lint-layout-search-forward-class-beginning)))
	(iface  (my-lint-layout-search-forward-interface-beginning)))
    (cond
     ((and class iface)
      (goto-char (min class iface)))
     ((or class iface)
      (goto-char (or class iface))))))

(defun my-lint-layout-generic-class-count (&optional prefix)
  "Count Classes and interfaces in one file"
  (let (count)
    (while (my-lint-layout-generic-class-forward)
      (unless (looking-at ".*PHPUnit\\|Exception")
	(if count
	    (incf count)
	  (setq count 1))))
      (when (and count
	       (> count 1))
      (my-lint-layout-message
       (format "multiple classes or interfaces in same file: %d" count)
       prefix))))

(defun my-lint-layout-conditional-above-p ()
  "Check if fif/else line is above."
    (save-excursion
      (goto-char (line-beginning-position))
      (or (my-lint-layout-looking-at-control-statement-p)
	  ;; FIXME: does't work for multiline if ( .... )
	  ;; Only few lines backward
	  (unless (zerop (skip-chars-backward "^{" (* 80 4)))
	    (forward-line -1)
	    (my-lint-layout-looking-at-control-statement-p)))))

;;; ........................................................... &print ...

(defun my-lint-layout-php-check-multiple-print (&optional prefix)
  "Check multiple print statements."
  (let (str)
    (while (re-search-forward
	    ;;  3 x threshold
	    `,(concat
	       "^[ \t]*"
	       ;;  print '
	       ;;  print "
	       ;;  print $variable
	       ;;  -- Ignore
	       ;;  print $class->method();
	       "\\(?:print\\|echo\\)[ \t]*(?[ \t]\\([\"']\\|[$][^- <>;\t\r\n]+[ \t.,;]\\)"
	       ".*\n"
	       ".*\\<"
	       "\\(?:print\\|echo\\)[ \t]*(?[ \t]\\([\"']\\|[$][^- <>;\t\r\n]+[ \t.,;]\\)"
	       ".*\n"
	       ".*\\<"
	       "\\(?:print\\|echo\\)[ \t]*(?[ \t]\\([\"']\\|[$][^- <>;\t\r\n]+[ \t.,;]\\)")
	    nil t)
      ;; (setq str (match-string 0))
      (my-lint-layout-message
       `,(concat "[code] possible maintenance problem, "
		 "multiple output calls, HERE doc recommended")
       prefix))))

(defsubst my-lint-layout-php-print-command-forward-1 ()
  "Find print or echo command."
  (re-search-forward
   ;; text/html; charset=utf-8
   "^[ \t]*\\(print\\|echo\\)[ \t]*[(\"'][^;]+;" nil t))

(defsubst my-lint-layout-php-print-command-forward ()
  "Search print or echo command. Return beginning point of match."
  (let (beg)
    (when (my-lint-layout-php-print-command-forward-1)
      (setq beg (match-beginning 0))
      (unless (looking-at "[; \t]*$")
	(when (re-search-forward ";[ \t]*$" nil t)))
      beg)))

(defun my-lint-layout-php-check-multiline-print (&optional prefix)
  "Check long print statements, when there is no $var anywhere.

print 'this' .
      'and' .
      ....
      ;
"
  (let (beg
	str
	lines)
    (while (setq beg (my-lint-layout-php-print-command-forward))
      (setq str (buffer-substring beg (point)))
      (unless (string-match "<<<" str)
	(setq lines (my-lint-layout-count-lines-in-string str))
	(when (> lines 3)
	  (my-lint-layout-message
	   "possible maintenance problem, continued print, HERE doc recommended"
	   prefix
	   (- (my-lint-layout-current-line-number) lines)))))))

;;; ......................................................... &php-sql ...

(defsubst my-lint-layout-php-var-forward-1 ()
  "Find variable definition."
  (re-search-forward "^[ \t]*\\([$][a-z].*=[^;]+\\)" nil t))

(defsubst my-lint-layout-php-var-sql-forward-1 ()
  "Find SQL content in variable.
Point is at end of variable after search.
Return variable content string."
  (let (ret
	point
	str)
    (while (and (null ret)
		(my-lint-layout-php-var-forward-1))
      (setq point (match-beginning 0)
	    str   (match-string 0))
      (when (or (string-match "\\<INSERT[ \t\r\n]+INTO\\>" str)
		(and (string-match "\\<SELECT[ \t\"'$]" str)
		     (string-match "\\<FROM[ \t\"'$]" str)))
	(setq ret str)))
    str))

(defun my-lint-layout-php-check-multiline-sql (&optional prefix)
  "Check long SQL statements.

    $insertCustomerQuery =
    \" INSERT INTO   customer              \" .
    \"              (firstName             \" .
    \"               , lastName            \" .
    \"               , address             \" .
    ...
"
  (let (str
	lines)
    (while (setq str (my-lint-layout-php-var-sql-forward-1))
      (when (and (not (string-match "<<<" str))
		 (string-match "^\\([ \t]*\".*[\r\n]\\)+" str)
		 (> (my-lint-layout-count-lines-in-string
		     (match-string 0 str))
		    3))
	(setq lines (my-lint-layout-count-lines-in-string str))
	(my-lint-layout-message
	 "possible SQL maintenance problem, HERE doc syntax recommended."
	 prefix
	 (- (my-lint-layout-current-line-number) lines))))))

;;; ............................................................. &xml ...

(defun my-lint-layout-check-xml-tags-lazy (tag &optional prefix)
  "Check lazy `<?' when it should read `<?TAG'."
  (let (str)
    (while (re-search-forward my-lint-layout-generic-xml-tag-regexp nil t)
      (setq str (match-string 0))
      (when (string= str "<?")
	(unless (looking-at tag)
	  (my-lint-layout-message
	   (format
	    "unknown opening short xml tag, expecting long <?tag (found: %s)"
	    (my-lint-layout-current-line-string))
	   prefix))))))

(defun my-lint-layout-php-check-xml-tags-lazy (&optional prefix)
  "Check <?php and <?xml tag."
  (my-lint-layout-check-xml-tags-lazy "php\\|xml"))

(defun my-lint-layout-generic-xml-tags-check-main (&optional prefix)
  "Check multiple invocations like:
<?
?>
<?
?>"
  (let ()
    (while (re-search-forward "[?]>[ \t\r\n]*<[?]" nil t)
      (my-lint-layout-message
       "multiple tag invocations one after another"
       prefix))))

(defun my-lint-layout-generic-check-control-statements (&optional prefix)
  "Keywords and preceding newlines."
  (let ()
    (while (re-search-forward
	    `,(concat
	       "\n[ \t]*\\([^ \t\r\n{]\\).*\n[ \t]*"
	       my-lint-layout-generic-control-statement-start-regexp
	       "[ \t]*(.*)")
	    nil t)
      ;; Ignore comments, xml-tags, starting brace
      (unless (save-excursion
		(goto-char (match-beginning 1))
		;; (my-lint-layout-current-line-string)
		(looking-at "[*/#]\\|[<>][?]\\|.*{"))
	(my-lint-layout-message
	 (concat
	  "[newline] no empty line found between "
	  "control statement and code above")
	 prefix)))))

(defun my-lint-layout-generic-check-block-end-and-code (&optional prefix)
  "Block end followed by code immediately after.

	}
	return;
"
  (let (line
	str)
    (while (re-search-forward
	    my-lint-layout-generic-brace-and-code-regexp nil t)
      (setq str (buffer-substring (match-beginning 1) (match-end 1)))
      (when (and
	     (not (my-lint-layout-string-comment-p str))
	     (not (string-match
		   (concat
		    my-lint-layout-generic-control-statement-regexp
		    "\\|"
		    my-lint-layout-generic-xml-tag-regexp)
		   str)))
	(setq line (my-lint-layout-current-line-number))
	(my-lint-layout-message
	 "[newline] no empty line found between '}' and next code line"
	 prefix)))))

(defun my-lint-layout-generic-check-comment-statement-examine
  (str &optional prefix)
  "Examine comment STR and text around point."
  ;; Allow "//////" "*******" "###########"
  (when (looking-at "[^ /*#\t\r\n]")
    (my-lint-layout-message
     "[comment] no space between comment marker and text"
     prefix))
  ;; Peek previous line
  (save-excursion
    (forward-line -1)
    (unless (looking-at (concat re "\\|^[ \t\r]*$\\|.*{"))
      (my-lint-layout-message
       "[comment] no empty line before comment start"
       prefix
       (1+ (my-lint-layout-current-line-number))))))

(defun my-lint-layout-generic-check-comment-statements (&optional prefix)
  "Check comment markers."
  (let ((re "^[ \t]*\\([#]\\|//\\|/\\*+\\)")
	str)
    (while (re-search-forward re nil t)
      (setq str (match-string 1))
      (cond
       ;; CSS text reads: #button { border: 1px; }
       ((and (string= "#" str)
	     (looking-at "[_a-z].*{")))
       ((not (string-match "#" str))
	(my-lint-layout-generic-check-comment-statement-examine
	 str prefix))))))

(defun my-lint-layout-generic-doc-above-p ()
  "Check if phpdoc is in above line."
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-backward "  \t\r\n") ;;  At the end of "*/"
    (goto-char (line-beginning-position))
    (looking-at "^[ \t]*[*]/[ \t]*$")))

(defsubst my-lint-layout-php-re-search-forward-doc-keyword ()
  "Search `my-lint-layout-php-doc-location-regexp'."
  (re-search-forward
   my-lint-layout-php-doc-location-regexp
   nil t))

(defsubst my-lint-layout-java-re-search-forward-doc-keyword ()
  "Search `my-lint-layout-php-doc-location-regexp'."
  (re-search-forward
   my-lint-layout-java-doc-location-regexp
   nil t))

(defsubst my-lint-layout-generic-re-search-forward-doc-keyword ()
  "Search `my-lint-layout-php-doc-location-regexp'."
  (cond
   ((eq 'php-mode (my-lint-layout-code-type-p))
    (my-lint-layout-php-re-search-forward-doc-keyword))
   ((eq 'java-mode (my-lint-layout-code-type-p))
    (my-lint-layout-java-re-search-forward-doc-keyword))))

(defsubst my-lint-layout-xml-element-beginning (tag &optional end)
  "Search start of XML element TAG."
  (re-search-forward
   ;; <form ... atrribute="" ...>
   (concat "<[ \t\r\n]*" tag "\\([ \t\r\n]*\\|[ \t\r\n][a-z].*\\)?>")
   end t))

(defsubst my-lint-layout-xml-element-end (tag &optional end)
  "Search end of XML element TAG."
  (re-search-forward
   (concat "<[ \t\r\n]*/[ \t\r\n]*" tag "[ \t\r\n]*>")
   end t))

(defun my-lint-layout-php-check-input-form-string
  (string &optional line prefix)
  "Check for STRING."
  (let (str
	beg
	end)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (while (my-lint-layout-xml-element-beginning "textarea")
	;; "How to limit the number of characters entered in a textarea
	;;  in an HTML form and other notes on textarea elements"
	;; http://www.cs.tut.fi/~jkorpela/forms/textarea.html
	(my-lint-layout-message
	   (concat
	    "[code] FORM security, textarea is unlimited by W3C standard. "
	    "Injection attacks must be checked manually")
	   prefix
	   (+ line (1- (my-lint-layout-current-line-number))))))))

(defun my-lint-layout-php-check-input-form-main (&optional prefix)
  "Check input form."
  (let (line
	beg
	end)
  (while (my-lint-layout-xml-element-beginning "form")
    (setq beg (match-beginning 0))
    (when (my-lint-layout-xml-element-end "form")
      (setq end (match-end 0))
      (setq line (my-lint-layout-current-line-number))
      (my-lint-layout-php-check-input-form-string
       (buffer-substring beg end) line prefix)))))

(defun my-lint-layout-php-here-doc-nearby-p (&optional point)
  "Return location if HERE doc is around `point'."
  (or point
      (setq point (point)))
  (let* ((range 400) ; 400 = approx 5 x 80 line range
         (min (max (- point range) (point-min)))
         (found
          (string-match
           "<<<"
          (buffer-substring-no-properties
           min
           (min (+ point range) (point-max))))))
    (if found
        (+ min found))))

(defun my-lint-layout-php-check-sql-kwd-statements (&optional prefix)
  "Check SQL statements and keywords in uppercase."
  (let ((re `,(concat
	       "\\<\\(insert[ \t\r\n]+into"
		     "\\|delete[ \t\r\n]+from"
	       "\\)\\>"))
	str
	match
	point
	beg
	end)
  (while (re-search-forward re nil t)
    (setq beg  (match-beginning 0)
	  match (match-string 1))
	  ;; end   (my-lint-layout-search-forward-ending-semicolon))
    (unless (my-lint-layout-string-uppercase-p match)
      (my-lint-layout-message
       (format
	"[code] SQL keyword not uppercase: %s" match)
       prefix))
    (cond
     ((string-match "insert" match)
      (goto-char beg)
      (when (re-search-forward
	     "\\<values\\>"
	     ;;  Check continued statement
	     ;;  $sql  =
	     ;;  $sql .=
	     ;;  ...
	     (max (+ beg 1500) (point-max))
	     t)
	(setq match (match-string-no-properties 0)
	      point (point))
	;; Where is the approximate ")" paren that terminates the INSERT
	(when (re-search-forward ")" (max (+ beg 1500) (point-max)) t)
	  (setq str (buffer-substring-no-properties beg (point)))
	  (when (and (> (my-lint-layout-count-char-in-string ?\n str) 3)
                     (not (my-lint-layout-php-here-doc-nearby-p point)))
	    (my-lint-layout-message
	     "possible SQL maintenance problem, HERE doc recommended"
	     prefix)))
	(unless (my-lint-layout-string-uppercase-p match)
	  (my-lint-layout-message
	   (format
	    "[code] SQL keyword not uppercase: %s" match)
	   prefix))
	(unless (re-search-backward ")" beg t)
	  (my-lint-layout-message
	   (concat
	    "[code] SQL portability; in INSERT, "
	    "missing column definitions in parens before VALUES")
	   prefix
	   (save-excursion
	     (goto-char point)
	     (my-lint-layout-current-line-number)))))))
    (when end
      (goto-char end)))))

(defun my-lint-layout-generic-class-check-variables (&optional prefix)
  "Check class variables."
  (let (class-p
	max
	point
	str)
    (save-excursion
      (setq point (point))
      (setq class-p (my-lint-layout-search-forward-class-p))
      (goto-char point)
      ;;  one class - One file assumption. If there are no methods, this is
      ;;  pure variable class, like struct.
      (setq max (or (my-lint-layout-generic-search-forward-function-beginning)
		    (point-max)))
      (when class-p
	(goto-char point)
	(while (my-lint-layout-search-forward-variable-beginning max)
	  (setq str (my-lint-layout-current-line-string))
	  (cond
	   ((string-match "\\<var\\>" str) ; PHP
	    (my-lint-layout-message
	     (concat
	      "[code] deprecated 'var'; expecting "
	      "private, public etc. access modifiers")
	     prefix))
	   ((string-match
	     my-lint-layout-generic-access-modifier-regexp
	     str)) ;; OK, do nothing
	   (t
	    (my-lint-layout-message
	     (concat
	     "[code] possibly missing access modifier "
	     "like private, public etc.")
	     prefix))))))))

(defun my-lint-layout-java-check-doc-missing (&optional prefix)
  "Check missing documentation."
  (let (;;class-p
	str
	line)
    ;; (save-excursion
    ;;   (setq class-p (my-lint-layout-search-forward-class-p)))
    (while (my-lint-layout-generic-re-search-forward-doc-keyword)
      (setq str (my-lint-layout-current-line-string))
      (unless (my-lint-layout-generic-doc-above-p)
	(cond
	 ;; ((my-lint-layout-type-import-string-p str)
	 ;;  (my-lint-layout-message
	 ;;   "[doc] import possibly not documented"
	 ;;   prefix))
	 ((my-lint-layout-type-function-string-p str)
	  (my-lint-layout-message
	   "[doc] method possibly not documented"
	   prefix))
	 ((my-lint-layout-type-statement-string-p str)
	  (my-lint-layout-message
	   "[doc] variable possibly not documented"
	   prefix)))))))

(defun my-lint-layout-php-check-doc-missing (&optional prefix)
  "Check missing documentation."
  (let (class-p
	function-p
	str
	line)
    (save-excursion
      (setq class-p (my-lint-layout-search-forward-class-p)))
    (while (my-lint-layout-php-re-search-forward-doc-keyword)
      (setq str (my-lint-layout-current-line-string))
      (unless (my-lint-layout-generic-doc-above-p)
	(cond
	 ;; if (...)
	 ;;    require "this" . $var;
	 ((and (my-lint-layout-conditional-above-p)
	       (my-lint-layout-looking-at-variable-at-line-p)))
	 ((and (or (not class-p)
		   ;;  Only check outside of class
		   ;;
		   ;;  require 'this';
		   ;;
		   ;;  class name
		   ;;      function some ()
		   ;;         require 'not this';
		   (< (point) class-p))
	       (my-lint-layout-type-include-string-p str))
	  (my-lint-layout-message
	   "[doc] require or include possibly not documented"
	   prefix))
	 ((my-lint-layout-type-function-string-p str)
	  (my-lint-layout-message
	   "[doc] function possibly not documented"
	   prefix))
	 ;; Skip "function files" FIXME: ???
	 ((my-lint-layout-type-include-string-p str)
	  (my-lint-layout-message
	   "[doc] require or include not documented (non-class context)"
	   prefix))
	 ((my-lint-layout-type-statement-string-p str)
	  (my-lint-layout-message
	   "[doc] variable possibly not documented"
	   prefix)))))))

(defsubst my-lint-layout-php-indent-level (str)
  "Count indent."
  (when str
    (if (string-match "\t" str)
	(setq str (replace-regexp-in-string "\t" "        " str)))
    (string-match "^ *" str)
    (length (match-string 0 str))))

(defsubst my-lint-layout-generic-statement-brace-forward (&optional brace)
  "Find statement block start. Optionally closing BRACE end."
  ;;  Notice that BRACE is hre used in regexp.
  ;;
  ;; if ( preg_match("^[0-9]{1,9}$", $bfield ) )
  ;; {
  ;;
  (if brace
      (setq brace "}")
    (setq brace "{"))
  (let ((opoint (point))
	(skip-chars (concat "^" brace))
	found
	point)
    (if (eq (char-after)		; on brace, move forward
	    (if brace
		?}
	      ?{))
	(forward-char 1))
    (while (and (null found)
		(not (eobp))
		(skip-chars-forward skip-chars)
		(setq point (point)))
      ;; Ignore brace inside comments
      (unless (my-lint-layout-with-save-point
		(goto-char (line-beginning-position))
		(unless (my-lint-layout-looking-at-comment-point-p)
		  (setq found point)))
	(if (not (eobp))
	    (forward-char 1))))
    (if (looking-at "[{}]")
	found
      (goto-char opoint)		; Don't move
      nil)))

(defsubst my-lint-layout-php-statement-brace-end-forward (&optional col)
  "Find ending brace at `current-column' or COL"
  (or col
      (setq col (current-column)))
  (let (point)
    (catch 'done
      (while (re-search-forward "^[ \t]*\\(}\\)[ \t]*$" nil t)
	(setq point (point))
	(goto-char (match-beginning 1))
	(if (eq col (current-column))
	    (throw 'done (point)))
	(goto-char point)))))

(defun my-lint-layout-php-check-indent-string-check
  (str line &optional prefix base-indent match-str)
  "Check STR for correct indent and report LINE as error."
  (let ((i (my-lint-layout-php-indent-level str))
	(istep my-lint-layout-generic-indent-step))
    (when (numberp i)
      (cond
       ((and (or (null base-indent)
		 (< i (+ base-indent istep)))
	     (not (zerop (mod i 4))))
	(my-lint-layout-message
	 (format (concat "[code] possibly incorrect indent "
			 "at col %d where multiple of %d expected")
		 i
		 istep)
	 prefix))
       ((and base-indent
	     (not (zerop i))
	     (eq i base-indent))
	(my-lint-layout-message
	 (format "[code] possibly missing indentation at col %d" i)
	 prefix))))))

(defun my-lint-layout-php-check-keywords-case (keyword fullstr &optional prefix)
  "If statement: check proper and, or, true, false character case."
  (my-lint-layout-with-case
    (if (string-match
	 "<\\(AND\\|OR\\|FALSE\\|TRUE\\|NULL\\)\\>"
	 fullstr)
	(my-lint-layout-message
	 (format "[code] keyword in conditional; expecting lowercase '%s'"
		 (match-string 0 fullstr))
	 prefix
	 ;;  Point is at brace, refer to above line.
	 (1- (my-lint-layout-current-line-number))))))

(defsubst my-lint-layout-php-here-doc-p (str)
  "Check if statement is HERE document start."
  (if (string-match "[^<]<<+[ \t]*\\([A-Z]+\\)" str)
      (match-string 1 str)))

(defsubst my-lint-layout-php-here-doc-skip (str)
  "If STR contains HERE doc, skip to end of marker."
  (let ((here (my-lint-layout-php-here-doc-p str)))
    (if here
	(my-lint-layout-with-case
	  (re-search-forward
	   (format "^[ \t]*%s[ \t]*;" here)
	   nil
	   t)))))

(defun my-lint-layout-php-statement-brace-block-check
  (beg end &optional base-indent prefix)
  "Check brace block between BEG and END using BASE-INDENT"
  (or base-indent
      (setq base-indent (current-column)))
  (goto-char beg)
  (let (match
	indent
	line
	here
	str)
    (while (re-search-forward
	    "^\\([ \t]*\\)\\(\\([^ \t\r\n]+\\).*\\)"
	    end
	    t)
      (setq match  (match-string 0)
	    indent (match-string 1)
	    line   (match-string 2)
	    str    (match-string 3))
      (or (my-lint-layout-php-here-doc-skip line)
	  (my-lint-layout-php-check-indent-string-check
	   indent
	   str
	   prefix
	   base-indent
	   match)))))

(defun my-lint-layout-generic-check-indent-current
  (indent &optional prefix)
  "Check current point for INDENT. Optional message PREFIX."
  (let* ((istep my-lint-layout-generic-indent-step)
	 (i (current-column))
	 (even-p (zerop (mod i istep)))
	 ;; (eight-p
	 ;;  (and indent			; 8 based ident check
	 ;;       (zerop (mod indent 4))	; expexted 4
	 ;;       (zerop (mod i 8))		; but is 8
	 ;;       (memq i '(8 16 24 32 40 48 56 64 72 80))))
	 )
    ;;  Comments are a special case
    ;;
    ;;  /*
    ;;   *
    ;;   |
    ;;   Correct indent position: 1 + indentation
    ;;
    (cond
     ((and (and indent)
	   (looking-at "\\*")
	   (not (eq i (1+ indent))))
      (my-lint-layout-message
       (format "[code] indent, comment char '*' expected at col %d"
	       (1+ indent))
       prefix))
     ;; Disabled, because does not hand "else if" etc. continuation.
     ;; ((and (and indent)
     ;; 	   (looking-at "}")
     ;; 	   (not (eq i indent)))
     ;;  (my-lint-layout-message
     ;;   (format "[code] indent, ending '}' expected at col %d"
     ;; 	       (if (zerop (mod indent istep))
     ;; 		   indent
     ;; 		 (* istep (/ indent istep))))
     ;;   prefix))
     ((and (not (zerop i))
	   (not even-p))
      (my-lint-layout-message
       (format (concat "[code] indent, at col %02d, expect multiple of %d")
	       i
	       istep)
       prefix))
     ;; ((and eight-p			; skip
     ;; 	   (= i (+ indent 4))))
     ;; ((and eight-p
     ;; 	   (< i indent))
     ;;  (my-lint-layout-message
     ;;   (format "[code] indent, at col %02d, expect %02d (move right)"
     ;; 	       i (+ 4 indent))
     ;;   prefix))
     ((and indent
	   (< i indent))
      (my-lint-layout-message
       (format "[code] indent, at col %02d, expect %02d (move right)"
	       i indent)
       prefix))
     ((and indent
	   (> i indent))
      (my-lint-layout-message
       (format "[code] indent, at col %02d, expect %02d (move left)"
	       i indent)
       prefix)))))

(defun my-lint-layout-generic-check-indent-forward (indent &optional prefix)
  "Check that lines are indented correctly until next brace.
Use BASE-INDENT, optional message PREFIX."
  (while (and (forward-line 1)
	      (not (eobp))
	      (not (looking-at "^.*{\\|^[ \t]*}")))
    (cond
     ((my-lint-layout-looking-at-empty-line-p)) ; do nothing
     ((my-lint-layout-looking-at-comment-start-multiline-p) ; skip
      ;; Handled in my-lint-layout-generic-check-comment-multiline-stars
      (my-lint-layout-comment-skip-multiline))
     ((looking-at
       "^[ \t]*\\(new[ \t]+\\)?[a-zA-Z][._a-zA-Z0-9]+[ \t]*([^;]*$")
      ;; Only check starting line
      (skip-chars-forward " \t")
      (my-lint-layout-generic-check-indent-current indent prefix)
      ;;
      ;;   Ignore rest, for continued function calls:
      ;;
      ;;   funcall(arg,
      ;;           arg,
      ;;           arg);
      (my-lint-layout-code-statement-end-search))
     (t
      (skip-chars-forward " \t")
      (my-lint-layout-generic-check-indent-current indent prefix)))))

(defun my-lint-layout-generic-statement-brace-and-indent (&optional prefix)
  "Check that code is indented after each brace.
If point is at `point-min' then check also ending brace placement.
Optional message PREFIX."
  (let ((istep my-lint-layout-generic-indent-step)
	level
	expect-indent
	indent)
    ;; FIXME: start counting levels are we find starting braces.
    (if (eq (point) (point-min))
	(setq level 0))
    (while (my-lint-layout-generic-statement-brace-forward)
      (goto-char (line-beginning-position))
      (unless (looking-at "^[ \t]*\\(/[/*]\\|[*#]\\)")  ;Skip brace in comments
	(skip-chars-forward " \t")
	;; The starting line be initially incorrect
	(cond
	 ((my-lint-layout-generic-check-indent-current nil prefix)
	  (forward-line 1))		;Error
	 (t
	  (setq expect-indent (+ (current-column) istep))
	  (my-lint-layout-generic-check-indent-forward
	   expect-indent prefix)
	  ;; FIXME: only when we start counting.
	  ;; End brace check
	  ;; (when (and (looking-at "^[ \t]*}")
	  ;; 	     (setq expect-indent (- expect-indent istep))
	  ;; 	     (> expect-indent 0))
	  ;;   (skip-chars-forward " \t")
	  ;;   (my-lint-layout-generic-check-indent-current
	  ;;    expect-indent prefix))
	  ))))))

(defun my-lint-layout-php-check-statement-comment-above
  (&optional str prefix)
  "Check misplaced comment: just above continue statement.

// comment here
else
{
    code
}
"
  (goto-char (line-beginning-position))
  (skip-chars-backward " \t\r\n")
  (when (my-lint-layout-looking-at-comment-line-p)
    (my-lint-layout-message
     (concat "possibly misplaced comment. Expected inside "
	     (if str
		 (format "'%s' block" str)
	       "next brace block"))
     prefix)))

(defun my-lint-layout-php-check-statement-continue-detach (str &optional prefix)
  "At statement STR, like 'else', peek above line."
  (forward-line -1)
  (unless (looking-at "^[ \t]*}")
    (my-lint-layout-message
     (format "keyword '%s' is not attached to brace block"
	     str)
     prefix)))

(defun my-lint-layout-php-check-statement-brace-detach (str &optional prefix)
  "Check if there is empty lines between keyword and brace:
KEYWORD

\{
  ..."
  (when (string-match "\\([^ \t\r\n]+\\).*\r?\n[ \t]*[\r\n]+[ \t]*{" str)
    (my-lint-layout-message
     (format "keyword '%s' is not attached to brace block"
	     (match-string 1 str))
     prefix)))

(defsubst my-lint-layout-php-check-statement-continue-p (string)
  "Check STRING against statement continue keywords."
  (string-match
   my-lint-layout-generic-control-statement-continue-regexp
   string))

(defsubst my-lint-layout-php-statement-forward ()
  "Search control statement forward."
  (re-search-forward
   my-lint-layout-generic-statement-regexp-line
   nil
   t))

(defun my-lint-layout-generic-check-statement-start-2 (&optional prefix)
  "Check incorrect statement like:

if ( check );
{
    line
}"
  (let (str)
    (while (my-lint-layout-php-statement-forward)
      (when (looking-at ";")
	(my-lint-layout-message
	 (format "possibly misplaced semicolon: %s"
		 (my-lint-layout-current-line-string))
	 prefix)))))

(defsubst my-lint-layout-php-brace-statement-forward (&optional max)
  "Search statement with brace forward."
  (re-search-forward
   my-lint-layout-generic-statement-regexp-brace
   max
   t))

(defun my-lint-layout-generic-check-statement-end (&optional prefix)
  "Check end of line for ';' and whitespace."
  (let (str)
    (while (my-lint-layout-search-forward-variable-beginning)
      (setq str (my-lint-layout-current-line-string))
      (when (string-match "[ \t];[ \t]*$" str)
	;;  "$a = 12 ;"  vs. "$a = 12;"
	(my-lint-layout-message
	 "[code] extra whitespace before statement end(;)"
	 prefix)))))

(defun my-lint-layout-generic-check-statement-start (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
	 (php-p (my-lint-layout-code-php-p))
	 keyword
	 fullstr
	 point
	 kwd-point
	 indent
	 comment-p
	 continue-p
	 brace-end-p
	 statement-start-col
	 statement-line
	 brace-start-col
	 brace-start-line
	 brace-end-col
	 brace-end-point
	 brace-end-line)
    (while (my-lint-layout-php-brace-statement-forward)
      (setq point     (match-beginning 1) ;; Left margin
	    kwd-point (match-beginning 2) ;; keyword start
	    keyword   (match-string 2)    ;; Keyword
	    fullstr   (match-string 0)
	    indent    (my-lint-layout-with-save-point
			;; At brace line
			(goto-char (line-beginning-position))
			(if (looking-at "[ \t]+")
			    (match-string 0)
			  ""))
	    ;; Cursor is one char after brace, correct that
	    brace-start-col (1- (current-column)))
      (setq continue-p
	    (my-lint-layout-php-check-statement-continue-p keyword))
      (forward-char -1)  ;; At brace start
      (save-excursion
	(goto-char kwd-point)
	(setq brace-end-p (looking-at ".*{"))
	(setq statement-start-col (current-column)
	      statement-line      (my-lint-layout-current-line-number))
	(when continue-p
	  (goto-char point)
	  (my-lint-layout-php-check-statement-continue-detach keyword prefix)
	  (goto-char point)
	  (my-lint-layout-php-check-statement-comment-above keyword prefix)))
      (my-lint-layout-php-check-indent-string-check
       indent statement-line prefix)
      ;; (my-lint-layout-generic-statement-brace-forward)
      ;; brace-start-line (my-lint-layout-current-line-number))
      (my-lint-layout-generic-statement-brace-and-indent prefix)
      (my-lint-layout-php-check-statement-brace-detach fullstr)
      (when (and php-p
		 (string-match "\\<if\\>\\|\\<els.*if\\>" keyword))
	(my-lint-layout-php-check-keywords-case keyword fullstr prefix))
      ;; Brace placement check
      (cond
       ((and (eq my-lint-layout-generic-brace-style 'brace-end)
	     (not brace-end-p))
	(my-lint-layout-message
	 (format "[code] brace { not at previous line of keyword '%s'"
		 (or keyword ""))
	 prefix))
       ((and (not (eq my-lint-layout-generic-brace-style 'brace-end))
	     (not (eq statement-start-col brace-start-col)))
	(my-lint-layout-message
	 (format "[code] brace { not directly under keyword '%s', expect col %d"
		 (or keyword "")
		 statement-start-col)
	 prefix))))))

(defun my-lint-layout-generic-check-statement-start-brace-end
  (&optional prefix)
  "Set `my-lint-layout-generic-brace-style'."
  (let ((my-lint-layout-generic-brace-style 'brace-end))
    (my-lint-layout-generic-check-statement-start prefix)))

(defun my-lint-layout-generic-check-statement-start-lined-up
  (&optional prefix)
  "Set `my-lint-layout-generic-brace-style'."
  (let ((my-lint-layout-generic-brace-style))
    (my-lint-layout-generic-check-statement-start prefix)))

;;; ........................................................ &conflict ...

;; CVS reports lines like:
;;
;; <<<<<<< driver.c
;;     exit(nerr == 0 ? EXIT_SUCCESS : EXIT_FAILURE);
;; =======
;;     exit(!!nerr);
;; >>>>>>> 1.6
;;
(defconst my-lint-layout-vc-conflict-marker-regexp
  "^\\(<<<<<<<\\|>>>>>>>\\) [0-9a-zA-Z]\\|^=======$"
  "Version control conflict marker.")

(defun my-lint-layout-php-check-vc-conflict-marker (&optional prefix)
  "Check version control conflict markers marker."
  (save-excursion
    (while (re-search-forward
	    my-lint-layout-vc-conflict-marker-regexp
	    nil t)
      (my-lint-layout-message
       (format "[misc] possible unresolved conflict: %s"
	       (my-lint-layout-current-line-string))
       prefix))))

;;; ........................................................ &keywords ...

(defun my-lint-layout-php-check-keyword-spelling-lowercase
  (str &optional prefix)
  "Check lowercase."
  (unless (string= (downcase str)
		   str)
    (my-lint-layout-message
     (format "[misc] lowercase keyword expected for: %s" str)
     prefix)))

(defun my-lint-layout-php-check-keywords-error-opening-paren-leading
  (str &optional prefix)
  "Error: <keyword><space>(); Leading <space>."
  (my-lint-layout-message
   (format "[misc] in funcall, extra space before opening paren: %s"
	   str)
   prefix))

(defun my-lint-layout-php-check-keywords-error-opening-paren-trailing
  (str &optional prefix)
  "Error: <keyword>(<space>..."
  (my-lint-layout-message
   (format "[misc] in funcall, extra space after opening paren: %s"
	   str)
   prefix))

(defun my-lint-layout-php-check-keywords-error-closing-paren-leading
  (str &optional prefix)
  "Error: <keyword>(...<space>)"
  (my-lint-layout-message
   (format "[misc] in funcall, extra space before closing paren: %s"
	   str)
   prefix))

(defun my-lint-layout-php-check-keywords-main (&optional prefix)
  "Check correct lowercase spelling.
See `my-lint-layout-php-function-call-keywords-generic'
and `my-lint-layout-php-function-call-keywords-no-paren'."
  (let* ((re-paren
	  (concat my-lint-layout-php-function-call-keywords-generic
		  "\\([ \t]*\\)("))
	 (re-noparen
	  (concat my-lint-layout-php-function-call-keywords-no-paren
		  "\\([ \t]*\\)[('\"$]"))
	 (re (concat re-paren "\\|" re-noparen))
	 keyword
	 class-p
	 function-p
	 str
	 indent
	 line)
    (save-excursion
      (goto-char (point-min))
      (setq class-p (my-lint-layout-search-forward-class-p)))
    (while (re-search-forward re nil t)
      (setq str (match-string 0))
      (when (string-match "^\\(.*\\([ \t]*\\)\\)[('\"$]" str)
	(setq keyword (match-string 1 str)
	      indent  (match-string 2 str))
	(my-lint-layout-php-check-keyword-spelling-lowercase keyword prefix)
	(when (> (length indent) 0)
	  (my-lint-layout-php-check-keywords-error-opening-paren-leading
	   str prefix))
	(when (eq (char-before (point)) ?\()
	  (when (looking-at "[ \t]")
	    (my-lint-layout-php-check-keywords-error-opening-paren-trailing
	     str prefix))
	  (when (looking-at "[^)\r\n]+[ \t])")
	    (my-lint-layout-php-check-keywords-error-closing-paren-leading
	     (match-string 0) prefix)))))))

;;; ........................................................ &spelling ...

(defconst my-lint-layout-word-search-regexp
  "Licence\\|Lisen[cs]e\
\\|This file is part of <program>\
\\|<program> is free software\
"
  "Search common misspelled or template words.")

(defun my-lint-layout-php-check-words (&optional prefix)
  "Check words."
  (let (str)
    (while (re-search-forward
	    my-lint-layout-word-search-regexp
	    nil
	    t)
      (setq str (match-string 0))
      (my-lint-layout-message
       (if (string-match "<" str)
	   (format "[misc] possibly unfilled template: %s" str)
	 (format "[misc] mispelled word: %s" str))
       prefix))))

(defun my-lint-layout-php-check-words-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-php-check-words'."
  (my-lint-layout-with-point-min
    (my-lint-layout-php-check-words prefix)))

(defun my-lint-layout-php-check-words-buffer-interactive ()
  "Run `my-lint-layout-php-check-words-buffer' and show results."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-php-check-words-buffer)))

;;; ............................................................. &eof ...

(defvar my-lint-layout-eof-regexp
  (regexp-quote "End of file")
  "End of file marker text.")

(defun my-lint-layout-check-eof-marker (&optional prefix)
  "Check EOF marker."
  (save-excursion
    (goto-char (point-max))
    (my-lint-layout-with-case
      (unless (re-search-backward
	       my-lint-layout-eof-regexp
	       (min (point-min) (* 4 80))
	       t)
	(my-lint-layout-message
	 (format "[misc] no exact EOF marker found: '%s'"
		 my-lint-layout-eof-regexp)
	 prefix)))))

(defun my-lint-layout-check-eof-marker-interactive ()
  "Near the last line of file find text `my-lint-layout-eof-regexp'."
  (interactive)
  (my-lint-layout-check-eof-marker))

;;; ...................................................... &whitespace ...

(defun my-lint-layout-whitespace-extra-newlines (&optional msg prefix)
  "Check extra newlines after current point."
  (when (looking-at "\\(\\(?:[ \t]*\r?\n\\)+\\)")
    (let ((str (match-string 0))
	  (line (my-lint-layout-current-line-number)))
      (my-lint-layout-message
       (format "[newline] extra newline %d%s"
	       (my-lint-layout-count-lines-in-string str)
	       (or msg ""))
       prefix line))))

(defun my-lint-layout-whitespace-indent-space-only (&optional prefix)
  "Check indentation: spaces only, no tabs."
  (when (re-search-forward "^\t[^ \t\r\n]" nil t)
    (my-lint-layout-message
     (format
      "[whitespace] tab used for indentation at line %d"
      (my-lint-layout-current-line-number))
     prefix)))

(defun my-lint-layout-whitespace-indent-mixed (&optional prefix)
  "Check indentation: space + tab."
  (while (re-search-forward "^ +\t" nil t)
    (my-lint-layout-message
     `,(concat
	"[whitespace] space+tab, but expect tab+space. "
	"Possibly an artefact from editor.")
     prefix)))

(defun my-lint-layout-whitespace-trailing (&optional prefix)
  "Check for trailing whitespace."
  (while (re-search-forward "[ \t]+$" nil t)
    (my-lint-layout-message
     "[whitespace] trailing whitepace at the end of line"
     prefix)))

(defun my-lint-layout-whitespace-trailing-cr (&optional prefix)
  "Check for trailing CR (^M)."
  ;; If this is a Windows file, then skip. Check first and last line.
  (let ((dos (save-excursion
	       ;; FIXME this is for any buffer, but we could also
	       ;; check the Emacs EOL variable?
	       (and (goto-char (point-min))
		    (looking-at ".*\r")
		    (goto-char (point-max))
		    (looking-at ".*\r")))))
    (unless dos
      (while (re-search-forward "\r$" nil t)
	(my-lint-layout-message
	 "[whitespace] trailing whitepace (\\r i.e. ^M) at the end of line"
	 prefix)))))

(defun my-lint-layout-whitespace-multiple-newlines (&optional prefix)
  "Check extra newlines before point."
  (while (re-search-forward "^[ \t]*\r?\n\\([ \t]*\r?\n\\)+" nil t)
    (my-lint-layout-message
     (format "[newline] extra newline(s) %d found above"
	     (1- (my-lint-layout-count-lines-in-string
		  (match-string 0))))
     prefix)))

(defun my-lint-layout-whitespace-at-eob (&optional prefix)
  "Check extra or missing newline at the end of buffer."
  (goto-char (point-max))
  (when (re-search-backward "[^ \t\r\n]\\(\n\\)?" nil t)
    (cond
     ((string= "\n" (match-string 1))
      ;; eob trailing newlines?
      (forward-line 1)
      (my-lint-layout-whitespace-extra-newlines
       " at end of file" prefix))
     (t
      (my-lint-layout-message
       "[newline] missing newline from last line of file"
       prefix)))))

(defun my-lint-layout-check-whitespace (&optional prefix)
  "Check whitespace problems: eol, bob, eob from current point."
  (my-lint-layout-generic-run-list
   my-lint-layout-check-whitespace-functions prefix))

(defun my-lint-layout-check-whitespace-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-check-whitespace'."
  (my-lint-layout-with-point-min
    (my-lint-layout-check-whitespace prefix)))

(defun my-lint-layout-check-whitespace-buffer-interactive ()
  "Run `my-lint-layout-check-whitespace-buffer' and show results."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-check-whitespace-buffer)))

;;; ............................................................ &crlf ...

(defsubst my-lint-layout-line-ending-lf-1-p ()
  "Check if point forward contains one line with LF line ending."
  (re-search-forward "^\n\\|[^\r\n]\n" nil t))

(defun my-lint-layout-line-ending-lf-p ()
  "Check if buffer contains one line with LF line ending."
  (my-lint-layout-with-point-min
    (my-lint-layout-line-ending-lf-1-p)))

(defun my-lint-layout-generic-check-mixed-eol-crlf (&optional prefix)
  "Check mixed CR/LF combination in buffer."
  (save-excursion
    (goto-char (point-min))
    (when (my-lint-layout-line-ending-lf-1-p)
      (goto-char (point-min))
      (when (search-forward "\r" nil t)
	(my-lint-layout-message
	 "[file-format] CR LF at the end of line (first occurrance)"
	 prefix)))))

;;; .......................................................... &length ...

(defun my-lint-layout-check-line-length (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
	 (re (concat "^"
		     (make-string col ?\.)
		     "\\(.+\\)")))
    (while (and (re-search-forward re nil t)
		;; Ignore URLs, C:\paths and path/name lines
		(not (string-match
		      "://\\|[a-z]:[\\].+[\\]\\|/[a-zA-Z0-9][a-zA-Z0-9_]+/"
		      (match-string 0))))
      (my-lint-layout-message
       (format "line lenght past column %d: %s" col (match-string 1))
       prefix))))

(defun my-lint-layout-check-line-length-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-check-line-length'."
  (my-lint-layout-with-point-min
    (my-lint-layout-check-line-length)))

(defun my-lint-layout-check-line-length-buffer-interactive (&optional prefix)
  "Run `my-lint-layout-check-line-length-buffer' and show results."
  (interactive)
  (my-lint-layout-check-line-length-buffer))

;;; ..................................................... &gpl-license ...

(defsubst my-lint-layout-license-gpl-search-forward ()
  "Position point to License line."
  (re-search-forward "\\<GNU General Public License\\>" nil t))

(defun my-lint-layout-license-not-exists (&optional prefix)
  "Check if License exists."
  (unless (my-lint-layout-license-gpl-search-forward)
    (my-lint-layout-message
     "[licence] GNU General Public License not found."
     prefix 1)
    t))

(defun my-lint-layout-license-text (text &optional prefix)
  "Check that License TEXT exists."
  (unless (re-search-forward (regexp-quote text) nil t)
    (my-lint-layout-message
     (format "[licence] text not found: %s..." text)
     prefix 1)))

(defun my-lint-layout-license-check-main (&optional prefix)
  "Check License syntax.
Optional PREFIX is used add filename to the beginning of line."
  (when (my-lint-layout-buffer-data-p)
    (if (my-lint-layout-license-not-exists prefix)
	t
      ;; The order is important
      (my-lint-layout-license-text
       "published by the Free Software Foundation" prefix)
      (my-lint-layout-with-case
	(my-lint-layout-license-text
	 "WITHOUT ANY WARRANTY" prefix))
      (my-lint-layout-license-text
       "You should have received a copy" prefix)
      (my-lint-layout-license-text
       "http://www.gnu.org/licenses" prefix))))

(defun my-lint-layout-license-check-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-license-check-main'."
  (my-lint-layout-with-point-min
    (my-lint-layout-license-check-main prefix)))

(defun my-lint-layout-license-check-main-interactive (&optional prefix)
  "Call `my-lint-layout-license-check-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-license-check-buffer)))

;;; ....................................................... &copyright ...

(defsubst my-lint-layout-copyright-line-p ()
  "Check if current point is copyright line.
Should be called right after `my-lint-layout-copyright-search-forward'."
  ;; Copyright information
  (and (not (looking-at ".*information"))
       ;;  Foo Bar
       (looking-at "[ \t]+.*[a-z]")))

(defsubst my-lint-layout-copyright-search-forward ()
  "Position point to 'Copyright <text>' line."
  (let (moved)
    (while (and (re-search-forward
		 "\\<copyright\\>"
		 nil t)
		(setq moved t)
		(not (my-lint-layout-copyright-line-p))))
    (and moved
	 (my-lint-layout-copyright-line-p))))

(defsubst my-lint-layout-copyright-email-string-p ()
  "Check email address."
  (and (string-match "@" string)
       (string-match "<.+@.+>" string)))

(defsubst my-lint-layout-copyright-email-re-search-forward-p ()
  "Check email address."
  (save-excursion
    ;; In different lines
    ;; Copyright (C) 2009 Foo Bar
    ;;                    <foo@example.com>
    (re-search-forward
     "<.*@.*>"
     (min (+ (point) 160)
	  (point-max))
     t)))

(defun my-lint-layout-copyright-line-syntax (&optional prefix)
  "Check Copyright line syntax."
  (let ((string (my-lint-layout-current-line-string)))
    (when (and (not (looking-at " +\\((C)\\|&copy;\\)"))
	       (not (string-match "@copyright" string)))
      (my-lint-layout-message
       (format "[copyright] expecting (C) sign: %s" string)
       prefix))
    (unless (looking-at ".*[0-9][0-9][0-9][0-9]")
      (my-lint-layout-message
       (format "[copyright] missing year: %s" string)
       prefix))
    ;;  Tag "@copyright ....."
    (when (not (string-match "@copyright" string))
      (cond
       ((or (my-lint-layout-copyright-email-string-p)
	    (my-lint-layout-copyright-email-re-search-forward-p))
	;; ok
	nil)
       ;; <p>Copyright &copy; 2009 - Restaurant Le Crotte</p>
       ((and (not (string-match "&copy" string))
	     (not (string-match "<..?>" string))
	     (not (string-match "@" string))
	     ;; content="..."
	     (not (string-match "[a-z][ \t]*=" string)))
	(my-lint-layout-message
	 (format "[copyright] possibly missing email address: %s" string)
	 prefix))
       (t
	(my-lint-layout-message
	 (format "[copyright] missing <> around email address: %s" string)
	 prefix))))
    (when (and (looking-at ".*<\\(.+\\)>")
	       (string-match
		"foo\\|bar\\|quux\\|example"
		(match-string 1)))
      (my-lint-layout-message
       (format "[copyright] email looks like template: %s" string)
       prefix))
    (when (looking-at ".*,")
      (my-lint-layout-message
       (format
	"[copyright] only one person should be listed in Copyright line: %s"
	string)
       prefix))))

(defun my-lint-layout-copyright-check-main (&optional prefix)
  "Check Copyright syntax.
Optional PREFIX is used add filename to the beginning of line."
  (let (found)
    (when (my-lint-layout-copyright-search-forward)
      (setq found t)
      (my-lint-layout-copyright-line-syntax prefix))
    (unless found
      (my-lint-layout-message "[copyright] not found" prefix 1))
    found))

(defun my-lint-layout-copyright-check-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-copyright-check-main'."
  (my-lint-layout-with-point-min
    (my-lint-layout-copyright-check-main prefix)))

(defun my-lint-layout-copyright-check-main-interactive ()
  "Call `my-lint-layout-copyright-check-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-copyright-check-buffer)))

;;; ....................................................... &changelog ...

(defconst my-lint-layout-changelog-item-regexp
  "^[ \t][*]\\( *\\)\\([^ :()\t\r\n]+\\)")

(defconst my-lint-layout-changelog-wordlist-regexp
  "\\(\
\\<[a-z]+ed\\>\
\\|<[a-z]+ing\\>\
\\)")

(defun my-lint-layout-word-tense (word message) ;Primitive
  "Check non-active tense."
  (when (and word
	     (string-match "\\(ed\\|ing\\)$" word))
    (my-lint-layout-message message prefix)))

(defun my-lint-layout-changelog-wording (&optional prefix)
  "Search for words in non-active tense."
  (while (re-search-forward my-lint-layout-changelog-wordlist-regexp nil t)
    (my-lint-layout-message
     (format "[changelog] word possibly in wrong tense '%s'"
	     (match-string 0))
     prefix)))

(defun my-lint-layout-changelog-file-items (&optional prefix)
  "Check ChangeLog syntax. The wording: Add, Modify, Change ..."
  (let (change
	word)
    (while (re-search-forward "(\\([^)\r\n]+\\)):" nil t)
      (setq change (match-string 1)
	    word   (and (looking-at " *\\([^ \t\r\n]+\\)")
			(match-string 1)))
      (when word
	(my-lint-layout-word-tense
	 word
	 (format "[changelog] change marker, wrong tense of verb '%s'" word)))
      (when (looking-at "  ")
	(my-lint-layout-message
	 (format
	  "[changelog] change marker, extra spaces after '(%s):'"
	  change)
	 prefix))
      (unless (looking-at "[ \r\n]")
	(my-lint-layout-message
	 (format
	  "[changelog] change marker, need one space after '(%s):'"
	  change)
	  prefix)))))

(defun my-lint-layout-changelog-file-bullet (&optional prefix)
  "Check ChangeLog syntax. The filename line:

  * application/template/overview.php: Add new file.

Optional PREFIX is used add filename to the beginning of line."
  (let (indent
	word
	file)
    (while (re-search-forward my-lint-layout-changelog-item-regexp nil t)
      (setq indent (match-string 1)
	    file   (match-string 2)
	    word   (or (and (looking-at
			     ":?.*([^)\r\n]+)[: ]+\\([^ \t\r\n]+\\)")
			    (match-string 1))
		       (and (looking-at ":?[ \t]*\\([^ \t\r\n]+\\)")
			    (match-string 1))))
      (when (looking-at "?:  ")
	(my-lint-layout-message
	 "[changelog] at *, extra space after pathname"
	 prefix))
      (when (looking-at ":?[ \t]*([^)\r\n]+):  ")
	(my-lint-layout-message
	 "[changelog] at *, extra space after (marker):"
	 prefix))
      (when (looking-at ":?[ \t]*([^)\r\n]+):[^ \t\r\n]")
	(my-lint-layout-message
	 "[changelog] at *, no space after (marker):"
	 prefix))
      (when (and (not (looking-at ":?[ \r\n]")))
	(my-lint-layout-message
	 "[changelog] at *, need one space after pathname"
	 prefix))
      (when word
	(my-lint-layout-word-tense
	 word
	 (format "[changelog] at *, wrong tense of verb '%s'" word)))
      (when (and (string-match " " indent)
		 (not (string= " " indent)))
	(my-lint-layout-message
	 "[changelog] not exactly one space after character '*'"
	 prefix))
      (forward-line 1))))

(defun my-lint-layout-changelog-check-main (&optional prefix)
  "Check ChangeLog syntax.
Optional PREFIX is used add filename to the beginning of line."
  (my-lint-layout-generic-check-mixed-eol-crlf prefix)
  (my-lint-layout-changelog-file-bullet prefix)
  (my-lint-layout-changelog-file-items prefix)
  (my-lint-layout-changelog-wording prefix))

(defun my-lint-layout-changelog-check-standard-main (&optional prefix)
  "Check ChangeLog syntax. With standard checks."
  (my-lint-layout-changelog-check-main prefix)
  (my-lint-layout-check-whitespace-buffer prefix)
  (my-lint-layout-check-line-length prefix))

(defun my-lint-layout-changelog-check-main-interactive ()
  "Call my-lint-layout-changelog-check-main and other relevant checks."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-changelog-check-standard-main)))

;;; ........................................................... &brace ...

(defsubst my-lint-layout-php-brace-forward-1 ()
  "Move to start brace { with problems."
  ;; {
  ;;
  ;;    if ()
  (and (re-search-forward
	"[{][ \t]*\n[ \t]*\\(?:\r?\n[ \t\r\n]+\\)[^{]" nil t)
       (let ((point (point)))
	 (forward-char -1)
	 (not (looking-at 
	       `,(concat
		  "/"			; comment start
		  "\\|"
		  my-lint-layout-generic-control-statement-regexp)))
	 (list point 'beg))))

(defsubst my-lint-layout-php-brace-forward-2 ()
  "Move to end brace }"
  ;;
  ;;
  ;; return
  ;;
  ;; }
  (and (re-search-forward "[^{][ \t]*\n\\([ \t]*\r?\n[ \t\r\n]*\\)[}]" nil t)
       (list (point) 'end)))

(defsubst my-lint-layout-php-brace-forward-3 ()
  "Move to function brace."
  (and (re-search-forward
	(concat
	 "^[ \t]*\\([ \t\r\n]+\\)"
	 "\n[ \t]*\\(?:function\\|public\\|private\\|protected\\)")
	nil t)
       (list (point) 'function-before)))

(defsubst my-lint-layout-php-brace-forward-4 ()
  "Move to empty brace {}"
  ;; if
  ;; {
  ;; }
  (and (re-search-forward "[{][ \t]*\r?\n[ \t]*[}]" nil t)
       (list (point) 'empty)))

(defsubst my-lint-layout-php-brace-forward-5 ()
  "Move to empty block } + immediate code."
  (and (re-search-forward
	(concat "}" my-lint-layout-generic-brace-and-code-regexp) ;; FIXME?
	nil t)
       (list (point) 'end-brace-and-code)))

(defun my-lint-layout-php-brace-forward-main ()
  "Move to first brace that contains problems."
  (let ((start (point))
	(point (point-max))
	ret-str
	str
	ret)
    (dolist (func '(my-lint-layout-php-brace-forward-1
		    my-lint-layout-php-brace-forward-2
		    my-lint-layout-php-brace-forward-3
		    my-lint-layout-php-brace-forward-4
		    my-lint-layout-php-brace-forward-5))
      (goto-char start)
      (multiple-value-bind (found type)
	  (funcall func)
	(when (and
	       found
	       (< found point)
	       (or (memq type '(empty))
		   (and
		    (setq str (match-string 1))
		    (setq count
			  (my-lint-layout-count-lines-in-string str))
		    (> count 0))))
	  (setq point found
		ret type
		ret-str str))))
    (goto-char point)
    (if ret
	(list ret ret-str))))

(defun my-lint-layout-php-brace-message (type line &optional count prefix)
  "Display message for TYPE at code LINE with optional COUNT lines."
  (let ((list
	 '((beg
	    "[newline] extra %d above. See previous brace '{'")
	   (end
	    "[newline] extra %d before ending brace '}'")
	   (function-before
	    "[newline] extra %d before function definition")
	   (empty
	    "[newline] empty brace block found")))
	elt)
    (when (setq elt (assoc type list))
      (multiple-value-bind (dummy format) elt
	(if (string-match "%" format)
	    (setq format (format format count)))
	(my-lint-layout-message format prefix line)))))

(defun my-lint-layout-generic-check-brace-extra-newline (&optional prefix)
  "Check forward for extra newlines after '{' and before '}'"
  (let (status
	line
	str)
    (while (setq status (my-lint-layout-php-brace-forward-main))
      (multiple-value-bind (type str) status
	(setq line (my-lint-layout-current-line-number))
	(my-lint-layout-php-brace-message
	 type
	 line
	 (and str
	      (my-lint-layout-count-lines-in-string str))
	 prefix)))))

;;; ............................................................. &sql ...

;; sql.el::sql-mode-ansi-font-lock-keywords

(defconst my-lint-layout-sql-keywords-reserved
  (concat
   "\\<"
   (regexp-opt
    '(
	"ALL" "AND" "ANY" "AS" "ASC" "BETWEEN" "BY" "CHECK" "CREATE"
	"CURRENT" "DEFAULT" "DELETE" "DESC" "DISTINCT" "EXISTS" "FOR"
	"FROM" "GRANT" "GROUP" "HAVING" "IN" "INSERT" "INTO" "IS"
	"LIKE" "NOT" "NULL" "OF" "ON" "OPTION" "OR" "ORDER" "PRIVILEGES"
	"PUBLIC" "SELECT" "SET" "TABLE" "TO" "UNION" "UNIQUE"
	"UPDATE" "USER" "VALUES" "VIEW" "WHERE" "WITH"
	"BEGIN" "CLOSE" "COMMIT" "MODULE" "SCHEMA"
	"PRIMARY" "PROCEDURE" "REFERENCES" "ROLLBACK"
	"CURSOR"
	"DECLARE" "END" "ESCAPE"
	)
      t) "\\>")
  "SQL standard reserved keywords.")

(defconst my-lint-layout-sql-keywords-function-list
  '(
    "avg"
    "bit_length"
    "cast"
    "char_length"
    "character_length"
    "coalesce"
    "concat"
    "convert"
    "count"
    "distinct"
    "extract"
    "length"
    "lower"
    "max"
    "min"
    "nullif"
    "octet_length"
    "position"
    "substring"
    "sum"
    "translate"
    "trim"
    "upper"
;; FIXME
;;     (concat
;;     "trim"
;;     "\\(?:[ \t\r\r]*([ \t\r\r]*"
;;           "\\(?:leading\\|trailing\\|both\\)"
;;     "\\)?")
;;     (concat
;;      "substring"
;;      "\\(?:[ \t\r\r]*([ \t\r\r]*"
;;            "\\(?:from[ \t\r\r]+[0-9]+"
;;                  "\\(?:[ \t\r\r]+for[ \t\r\r]+[0-9]+\\)?\\)"
;;      "\\)?")
    )
  "SQL standard reserved keywords for functions, list.")

(defconst my-lint-layout-sql-keywords-function-regexp
  (concat
   "\\<\\(?:"
   (regexp-opt
    my-lint-layout-sql-keywords-function-list)
   "\\)\\>")
  "SQL standard reserved keywords for functions, OR regexp.")

(defconst my-lint-layout-sql-keywords-column-mysql
  (concat
   "\\<"
   (regexp-opt
    '("auto_increment"
      "unsigned"
      "zerofill"
      "current_timestamp"
      "default"
      ) t) "\\>")
  "MySQL column keywords.")

(defconst my-lint-layout-sql-keywords-create-table-other
  (concat
   "\\<"
   (regexp-opt
    '(;; create table xxx (...) engine = innodb;
      "engine"
      "innodb"
      ) t) "\\>")
  "Non-standard SQL keywords in create table.")

(defconst my-lint-layout-sql-keywords-sql92-for-column-word-re
  '("not[ \t\r\n]+null"
    "primary[ \t\r\n]+key"
    "references"
    "match\\(?:[ \t\r\n]+\\(?:unique\\|partial\\|full\\)\\)?"
    "overlaps"
    "unique")
  "SQL-92 column definition keywords in CREATE TABLE statement.
List of word regexps.")

(defconst my-lint-layout-sql-keywords-sql92-for-column
  (concat
   "\\<\\(?:"
   (mapconcat
    'concat
    my-lint-layout-sql-keywords-sql92-for-column-word-re
    "\\|")
   "\\)\\>")
  "SQL-92 column definition keywords in CREATE TABLE statement.
One ORing regexp.")

(defconst my-lint-layout-sql-keywords-from-statement
  (list
   (concat
    "\\(?:\\<natural[ \t\r\n]+\\)?"	;optional "natural"
	  "\\(?:"
	      "\\(?:\\(?:left\\|right\\|full\\)[ \t\r\n]+\\)?" ;optional words
	      "\\<outer[ \t\r\n]+"      ;required "outer"
	    "\\)"
     "join")
    (concat
     "\\(?:\\<natural[ \t\r\n]+\\)?"	;optional "natural"
       "\\(?:\\<inner[ \t\r\n]+\\)?"	;optional "inner"
       "join")				;required "join"
    "ancestor"
    "ancestor_of"
    "child"
    "cross[ \t\r\n]+join"
    "all"
    "and"
    "any"
    "as"
    "between"
    "case"
    "corresponding[ \t\r\n]+by"
    "current_date"
    "current_time"
    "current_timestamp"
;;; "day"
    "else"
    "end"
    "escape"
    "except\\(?:[ \t\r\n]+distinct\\)?"
    "exists"
    "false"
    "for"
    "from"
    "group[ \t\r\n]+by"
    "having"
;;;    "hour"
    "in"
    "interval"
    "intersect"
    "is[ \t\r\n]+null"
    "is[ \t\r\n]+not[ \t\r\n]+null"
    "like"
    "local"
;;;    "minute"
    "not"
    "null"
    "on"
    "or"
    "order[ \t\r\n]+by"
;;;    "second"
    "parent"
    "parent_of"
    ;; SQL:1999 regular expressions
    "similar[ \t\r\n]+to"
    "some"
    "time[ \t\r\n]+zone"
    "timezone_hour"
    "timezone_minute"
    "true"
    "unique"
    "union\\(?:[ \t\r\n]+\\(?:all|distinct\\)\\)?"
    "unknwown"
    "using"
    "when"
;;; "year"
    )
  "List of regexp keywords appearing in FROM part of SELECT.")

(defconst my-lint-layout-sql-keywords-from-statement-regexp
  (concat
   "\\<\\(?:"
   (mapconcat
    'concat
    my-lint-layout-sql-keywords-from-statement
    "\\|")
   "\\)\\>")
  "OR regexp of `my-lint-layout-sql-keywords-from-statement'.")

(defconst my-lint-layout-sql-keywords-sql92-data-types
  (concat
   "\\<\\(?:"
   (regexp-opt
    '(
      "bit varying"
      "bit"
      "char varying"
      "char"
      "character varying"
      "character"
      "currency"
      "date"
      "dec"
      "decimal"
      "double precision"
      "float"
      "int"
      "integer"
      "interval"
      "money"
      "national char varying"
      "national char"
      "national character"
      "nchar varying"
      "nchar"
      "numeric"
      "real"
      "smallint"
      "time"
      "timestamp"
      "varchar"
      ))
   "\\)\\>")
  "SQL standard keywords for reserved data types.")

;; BIT data type in MS SQL Server stores a bit of data (0 or 1) and
;; does not correspond to previously described SQL99 BIT. The literal
;; value for bit is a single character from its range optionally
;; enclosed into single quotes.
;;
;; SMALLINT is virtually same as INTEGER, but maximum precision can be
;; smaller than that for INTEGER.

(defconst my-lint-layout-sql-keywords-sql99-data-types
  (concat
   "\\<\\(?:"
   (regexp-opt
    '("array"
      "boolean"
      "binary large object"
      "blob"
      "clob"
      "character large object"
      "lob"
      "large object"
      "list"
;;;      "money" FIXME check?
      "ref" ;; OID
      "row"
      "set"))
   "\\)\\>")
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-sql-types
  (concat
   "\\(?:"
   my-lint-layout-sql-keywords-sql92-data-types
   "\\|"
   my-lint-layout-sql-keywords-sql99-data-types
   "\\)")
  "SQL standard reserved keywords for data types.")

(defconst my-lint-layout-sql-keywords-sql-type-abbreviations
  (concat
   "\\<\\(?:"
   (regexp-opt
    '("char"
      "int"
      "dec"))
   "\\)\\>")
  "SQL standard reserved data types (abbreviations).")

(defsubst my-lint-layout-sql-statement-end-forward ()
  "Search next SQL statement end (semicolon).
Only SQL standrd comments are recognized. Not non-standard '#' etc.
Examples of ending lines:

o   <statement> );
o   ;
o   INSERT .... ( ); -- last comment
"
  (re-search-forward
    `,(concat
       "^[ \t]*)?;[ \t]*\\(?:--+.*\\)?$"
       "\\|)?;[ \t]*\\(?:--+.*\\)?$")
    nil t))

(defsubst my-lint-layout-sql-insert-into-forward ()
  "Search INSERT INTO forward.
The submatches are as follows. The point is at '!':

    INSERT INTO table <nothing>   VALUES (<values>) ;
    INSERT INTO table (<columns>) VALUES (<values>) ;
    |---------- |----  |--------         !
    1           2      3                 Point

Note, that the statement does not necessarily have VLAUES part.
This can be tested with `looking-at' at the position of point:

    INSERT INTO table (<values>) ;
				 |
				 Point"
  ;; FIXEME; This does not work if the data contains ")"
  (re-search-forward
   `,(concat
      "^[ \t]*"
      "\\(insert[ \t\r\n]+into\\>\\)"       ; 1 keyword
      "[ \t\r\n]+"
      "\\([^ \t\r\n]+\\)"                   ; 2 table name
      "\\([ \t\r\n]*([^)]+\\))*"
      "[ \t\r\n]*"                          ; 3 (values)
      "\\(VALUES\\)[ \t\r\n]+")             ; 4 keyword
   nil t))

(defun my-lint-layout-sql-check-indent (str &optional prefix)
  "Check left comma lines:

INSERT INTO kalleria_imgs
\(
col
, col
, col
..."
  (when (string-match "^\\([ \t]*\\)," str)
    (let ((i (length (match-string 1 str))))
      (if (or (not (> i 0))
	      (not (zerop (% i 4))))
	  (my-lint-layout-message
	   (format "[sql] possibly incorrect indentation at col %d" i)
	   prefix)))))

(defsubst my-lint-layout-sql-check-mixed-case
  (str message &optional prefix line)
  "Check if STR is mixedCase and signal error MESSAGE. PREFIX, LINE."
  (when (my-lint-layout-string-mixed-case-p str)
    (my-lint-layout-message message prefix line)))

(defsubst my-lint-layout-sql-check-all-uppercase
  (str message &optional prefix line)
  "Check STR against uppercase or signal error."
  (unless (my-lint-layout-string-uppercase-p str)
    (my-lint-layout-message message prefix line)))

(defsubst my-lint-layout-sql-check-charset-p (str)
  "Check STR against typical alphadigit charset"
  (not (string-match "[^a-zA-Z0-9_ \t\r\n]" str)))

(defsubst my-lint-layout-sql-check-charset
  (str message &optional prefix line)
  "Check STR against typical charset or signal error."
  (unless (my-lint-layout-sql-check-charset-p str)
    (my-lint-layout-message message prefix line)))

(defsubst my-lint-layout-sql-check-charset-column-p (str)
  "Check STR against typical charset."
  (not (string-match "[^a-zA-Z0-9,()_ \t\r\n]" str)))

(defsubst my-lint-layout-sql-check-column-charset
  (str message &optional prefix line)
  "Check STR against typical charset or signal error."
  (unless (my-lint-layout-sql-check-charset-column-p str)
    (my-lint-layout-message message prefix line)))

(defun my-lint-layout-sql-clean-comments-buffer (&optional point)
 "Remove all kind of comments from `point-min' or optional POINT forward."
 (or point
     (setq point (point-min)))
 (flet ((clean
	 (re)
	 (goto-char point)
	 (while (re-search-forward re nil t)
	   (replace-match ""))))
   (clean "[ \t]*#.*")   ;; MySQL hash comments
   (clean "[ \t]*--.*")  ;; Standard SQL comments
   (clean "[ \t]*/[*].*"))) ;; C-style comments /* .... */

(defsubst my-lint-layout-sql-clean-comments-string (string)
 "Remove all kind of comments at the end of STRING"
 (with-temp-buffer
   (insert string)
   (my-lint-layout-sql-clean-comments-buffer)
   (buffer-string)))

(defun my-lint-layout-sql-check-element-indent-check
  (indent &optional prefix line)
  "Check numeric INDENT."
  (cond
   ((and (eq 0 indent)
	 ;;  INSERT INTO
	 ;;  (  ...
	 (not (looking-at "(")))
    (my-lint-layout-message
     (format
      "[sql] possibly missing indentation at col %d"
      indent)
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number))))
   ((not (zerop (% indent step)))
    (my-lint-layout-message
     (format
      "[sql] possibly incorrect at col %d where multiple of %d expected"
      indent step)
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number))))))

(defun my-lint-layout-sql-check-element-indentation (&optional prefix line table)
  "Check left margin indentation of every line from current point.
LINE is added to current line number."
  (let ((step  my-lint-layout-generic-indent-step)
	indent)
    (save-excursion
      (goto-char (line-beginning-position))
      (while (not (eobp))
	;;  Do not check line that has terminating ';'
	;;  Or single "(" and ")" lines
	(when (and (not (looking-at "^.*;[ \t]*$"))
		   (not (looking-at "^[ \t]*$"))
		   (not (looking-at "^[ \t]*[();]+[ \t]*$"))
		   (looking-at "^\\([ \t]*\\)[^ \t]"))
	  (setq indent (length (my-lint-layout-expand-tabs
				(match-string 1))))
	  (my-lint-layout-sql-check-element-indent-check
	   indent prefix line))
	(forward-line 1)))))

(defun my-lint-layout-sql-check-insert-into-column-part
  (beg end &optional prefix line)
  "Check column definition in region BEG END. LINE. PREFIX."
  (let ((string (buffer-substring beg end))
	match
	word)
    (with-temp-buffer
      (insert string)
;;;      (display-buffer (current-buffer)) ;; FIXME
      (my-lint-layout-sql-clean-comments-buffer)
      (goto-char (point-min))
      (my-lint-layout-sql-check-element-indentation prefix line)
      (goto-char (point-min))
      ;; check every word: the column names
      (while (re-search-forward ".\\([^ ,()\t\r\n]+\\).?" nil t)
	(setq match (match-string 0)
	      word  (match-string 1))
	(my-lint-layout-sql-check-charset
	 word
	 (format "[sql] In INSERT, col non-alphadigit characters in %s" word)
	 prefix
	 (+ line (my-lint-layout-current-line-number)))
	(my-lint-layout-sql-check-mixed-case
	 match
	 (format
	  "[sql] In INSERT, col portability problem with mixed case: %s"
	  match)
	 prefix
	 (+ line (my-lint-layout-current-line-number)))
	))))

(defun my-lint-layout-sql-check-iso-date (&optional prefix line)
  "Check YYYY-MM-DD in string.
LINE is added to current line number."
  (let ((re `,(concat
	       "\\(.\\)"
	       "[0-9]\\{4,4\\}-[0-9][0-9]-[0-9][0-9]" ;; YYYY-MM-DD
	       ;; HH:MM:SS
	       "\\(?:[ \t]+[0-9][0-9]\\(?::[0-9][0-9]\\)\\{1,2\\}?\\)?"
	       "\\(.?\\)"))
	match
	open
	close)
    (flet ((test
	    (str)
	    (unless (string= "'" str)
	      (my-lint-layout-message
	       (format
		"[sql] incorrect or missing single quotes around date [%s]"
		match)
	       prefix
	       (+ (or line 0) (my-lint-layout-current-line-number))))))
      (while (re-search-forward re nil t)
	(setq match (match-string 0)
	      open  (match-string 1)
	      close (match-string 2))
	(test open)
	(test close)))))

(defun my-lint-layout-sql-check-null-literal (&optional prefix line)
  "Check literal NULL form `current-point'.
The value of LINE is added to current line. PREFIX."
  (while (re-search-forward "[\"\']NULL\\>." nil t)
    (my-lint-layout-message
     (format
      "[sql] in INSERT, possibly extra quotes around literal: %s"
      (match-string 0))
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number)))))

(defun my-lint-layout-sql-check-element-nbr-quotes (&optional prefix line)
  "Check that numbers are not inserted in quotes '123'.
The value of LINE is added to current line. PREFIX."
  (while (re-search-forward "[\"'][1-9][0-9]*[\"']" nil t)
    (my-lint-layout-message
     (format
      "[sql] in INSERT, possibly extra quotes around number: %s"
      (match-string 0))
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number)))))

(defun my-lint-layout-sql-check-element-double-quotes (&optional prefix line)
  "Check that values are enclosed in single quotes.
The value of LINE is added to current line. PREFIX."
  (while (re-search-forward "\"[^,\"\r\n]*\"" nil t)
    (my-lint-layout-message
     (format
      "[sql] in INSERT, SQL standard defines strings in single quotes: %s"
      (match-string 0))
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number)))))

(defun my-lint-layout-sql-check-insert-into-values-part
  (beg end &optional prefix line)
  "Check INSERT INTO <values> part."
  (let ((string (buffer-substring beg end))
	word)
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (my-lint-layout-sql-check-null-literal prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-iso-date prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-element-nbr-quotes prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-element-double-quotes prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-element-indentation prefix line))))

(defun my-lint-layout-sql-check-statement-insert-into-main
  (&optional prefix)
  "Check INSERT INTO statements. PREFIX."
  (let (point
	keyword
	table
	parenbeg
	paren-end
	line)
    (while (my-lint-layout-sql-insert-into-forward)
      (setq keyword   (match-string-no-properties 1)
	    table     (match-string-no-properties 2)
	    paren     (match-string-no-properties 3)
	    keyword-values (match-string-no-properties 4)
	    paren-beg (match-beginning 3)
	    paren-end (match-end 3)
	    line      (save-excursion
			(goto-char (match-beginning 0))
			(my-lint-layout-current-line-number)))
      (my-lint-layout-sql-check-all-uppercase
       keyword
       (format "[sql] In INSERT, keyword not uppercase: %s"
	       keyword)
       prefix line)
      (my-lint-layout-sql-check-mixed-case
       table
       (format "[sql] In INSERT, portability problem with mixed case: %s"
	       table)
       prefix line)
      (cond
       ;;  FIXME: RE-search
       ((string-match "values" keyword-values)
	(let ((beg (point))
	      end)
	  (my-lint-layout-sql-check-all-uppercase
	   keyword-values
	   (format "[sql] In INSERT, keyword not uppercase: %s" keyword)
	   prefix line)
	  (my-lint-layout-sql-check-insert-into-column-part
	   paren-beg paren-end prefix line)
	  (if (null (setq end
			  (my-lint-layout-sql-statement-end-forward)))
	      (my-lint-layout-message
	       "[sql] in INSERT, cannot find statement end marker(;)"
	       prefix)
	    (my-lint-layout-sql-check-insert-into-values-part
	     beg end prefix line))))
       (t
	(my-lint-layout-message
	 "[sql] in INSERT, column names not listed"
	 prefix)
	(my-lint-layout-sql-check-insert-into-values-part
	 paren-beg paren-end prefix line))))))

(defsubst my-lint-layout-statement-select-forward ()
  "Search INSERT INTO forward.
The submatches are as follows. The point is at '!':

    SELECT <select> FROM ...
    |----  |------- !
    1      2        Point"
  (re-search-forward
   `,(concat
      "^[ \t]*"
      "\\(select\\)"
      "[ \t\r\n]+"
      "\\([^;]+\\)"
      "[ \t\r\n]+"
      "\\(from\\)"
      "[ \t\r\n]+")
   nil t))

(defun my-lint-layout-sql-check-select-col-as-part
  (keyword string &optional prefix line)
  "Examine keyword 'AS' and rest of the STRING."
  (my-lint-layout-sql-check-all-uppercase
   keyword
   (format "[sql] In SELECT, keyword not uppercase: %s" keyword)
   prefix line)
  (cond
   ((string-match "^[ \t]*\"" string)) ;; ok
   ((string-match "^[ \t]*'" string)
    (my-lint-layout-message
     (format
      "[sql] in SELECT, double quotes suggested for portability in AS: %s"
      string)
     prefix line))
   (t
    (my-lint-layout-message
     (format "[sql] in SELECT, double quotes expected for AS alias: %s"
	     string)
     prefix line))))

(defun my-lint-layout-sql-check-select-col-part
  (string &optional prefix line)
  "Examine column STRING."
  (when (string-match
	 `,(concat
	    ;; <col word|expr> [<rest>]
	    ;; 1               2
	    "\\([^ ,\t\r\n]+\\)"
	    "[ \t\r\n]*"
	    "\\(.*\\)")
	 string)
    (let ((match (match-string 0 string))
	  (word  (match-string 1 string))
	  (rest  (match-string 2 string)))
      ;; FIXME: only first word is checked
      (my-lint-layout-sql-check-mixed-case
       word
       (format
	"[sql] In SELECT, portability problem with mixed case: %s"
	word)
       prefix line)
      (when (string-match "\\(\\<AS\\>\\)[ \t\r\n]+\\(.*\\)" rest)
	(my-lint-layout-sql-check-select-col-as-part
	 (match-string 1 rest)
	 (match-string 2 rest)
	 prefix line))
      )))

(defun my-lint-layout-sql-check-statement-select-display-part
  (beg end &optional prefix line)
  "Check SELECT <display> part."
  (let ((string (buffer-substring beg end)))
    (with-temp-buffer
      (insert string)
      (my-lint-layout-sql-clean-comments-buffer)
      (goto-char (point-min))
      (let (match
	    curline)
	(while (re-search-forward "[ \t]*\\([^,]+\\)" nil t)
	  (setq match    (match-string 1)
		curline  (+ line (1- (my-lint-layout-current-line-number))))
	  (when (looking-at ",.*[,;]")
	    (my-lint-layout-message
	     "[sql] in SELECT, possibly multiple columns(,) listed at same line"
	     prefix curline))
	  (my-lint-layout-sql-check-select-col-part
	   match prefix curline))))))

(defsubst my-lint-layout-sql-check-statement-select-from-part-keyword-case1
  (string &optional prefix line)
  "Signal lowecase keyword error."
  (my-lint-layout-message
   (format "[sql] in SELECT, FROM part possibly has non-uppercase keyword: %s"
	   string)
   prefix line))

(defun my-lint-layout-sql-check-statement-select-from-part-keyword-case
  (&optional prefix line)
  "Check SELECT ... FROM <from part> for known keywords."
  (my-lint-layout-with-case
    (dolist (re (list
		 my-lint-layout-sql-keywords-from-statement-regexp
		 my-lint-layout-sql-keywords-function-regexp))
      (when (stringp re)
	(let ((regexp (concat "\\(" re "\\)")))
	  (while (re-search-forward regexp nil t)
	    (my-lint-layout-sql-check-statement-select-from-part-keyword-case1
	     (match-string 1)
	     prefix
	     (or line
		 (my-lint-layout-current-line-number)))))))))

(defun my-lint-layout-sql-check-statement-select-from-part-equals
  (&optional prefix line)
  "Check equal '=' lines."
  (let (sign
	match
	before
	after
	curline
	col)
    (while (re-search-forward "\\(.\\)\\(<>\\|=[<>]?\\)\\(.\\)" nil t)
      (setq match  (match-string 0)
	    before (match-string 1)
	    sign   (match-string 2)
	    after  (match-string 3)
	    col    (1- (current-column))
	    curline (if line
			(+ line (1- (my-lint-layout-current-line-number)))
		      (my-lint-layout-current-line-number)))
      (unless (string-match "[ \t\r\n]" before)
	(my-lint-layout-message
	 (format "[sql] in SELECT, no space before '%s' sign at col %s"
		 sign col)
	 prefix curline))
      (unless (string-match "[ \t\r\n]" after)
	(my-lint-layout-message
	 (format "[sql] in SELECT, no space after '%s' sign at col %s"
		 sign col)
	 prefix curline))
      ;;  <column> = '<number>'
      (when (looking-at "[ \t]*[\"'][0-9]+[\"']")
	(my-lint-layout-message
	 (format
	  "[sql] in SELECT, possibly extra quotes to the right: %s"
	  (concat match (match-string 0)))
	 prefix curline)))))

(defun my-lint-layout-sql-check-statement-select-from-part-quotes
  (&optional prefix line)
  "Check equal use of double quotes. SQL standard requires single quotes."
  (let ((re  `,(concat
		"\\(<>\\|=[<>]?"
		"\\|\\<between"
		"\\|\\<and"
		"\\|\\<or"
		"\\|\\<like"
		"\\)[ \t]*\""))
	sign
	curline
	col)
    (while (re-search-forward re nil t)
      (setq sign    (match-string 1)
	    col     (match-beginning 1)
	    curline (if line
			(+ line (1- (my-lint-layout-current-line-number)))
		      (my-lint-layout-current-line-number)))
      (my-lint-layout-message
       (format "[sql] in SELECT, single quote expected at %s near %s\"%s"
	       col
	       sign
	       (if (looking-at "[^ \t\r\n]+")
		   (match-string 0)
		 ""))
       prefix curline))))

(defun my-lint-layout-sql-check-statement-select-from-part
  (beg end &optional prefix line)
  "Check SELECT display FROM <from part>."
  (let ((string (buffer-substring beg end))
	match
	word)
    (with-temp-buffer
      (insert string)
;;;      (display-buffer (current-buffer)) ;; FIXME
      (my-lint-layout-sql-clean-comments-buffer)
      (goto-char (point-min))
      (my-lint-layout-sql-check-statement-select-from-part-keyword-case
       prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-statement-select-from-part-equals
       prefix line)
      (goto-char (point-min))
      (my-lint-layout-sql-check-statement-select-from-part-quotes)
      ;; FIXME indentation checks
;;       (let (match)
;;      (while (re-search-forward "\\([^ ,\t\r\n]+\\)" nil t)
;;        (setq match (match-string 1))
;;        (my-lint-layout-sql-check-statement-select-from-part-keyword-case
;;         match prefix line)))
      )))

(defun my-lint-layout-sql-check-statement-select-main (&optional prefix)
  "Check all INSERT INTO lines. PREFIX."
  (let (point
	select
	select
	from
	fromp
	table
	beg
	end
	line)
    (while (my-lint-layout-statement-select-forward)
      (setq select    (match-string 1)
	    beg       (match-beginning 2)
	    end       (match-end 2)
	    from      (match-string 3)
	    fromp     (match-beginning 3)
	    line      (save-excursion
			(goto-char (match-beginning 0))
			(my-lint-layout-current-line-number)))
      (my-lint-layout-sql-check-all-uppercase
       select
       (format "[sql] In SELECT, keyword not uppercase: %s" select)
       prefix line)
      (unless (my-lint-layout-string-uppercase-p from)
	(my-lint-layout-message
	 (format "[sql] In SELECT, keyword not uppercase: %s" from)
	 (save-excursion
	  (goto-char fromp)
	  prefix
	  (my-lint-layout-current-line-number))))
      (my-lint-layout-sql-check-statement-select-display-part
       beg end prefix line)
      (when (re-search-forward ")[ \t]*;\\|^[ \t]*;" nil t)
	(my-lint-layout-sql-check-statement-select-from-part
	 fromp (point) prefix line)))))

(defsubst my-lint-layout-sql-create-table-error-data-type-lower
  (string &optional prefix line)
  "Signal lowercase keyword error."
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, keyword not uppercase: %s" string)
   prefix
   line))

(defsubst my-lint-layout-sql-create-table-error-data-type-abbrev
  (abbrev string &optional prefix line)
  "Signal abbreviated keyword error."
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, abbreviated '%s' keyword: %s"
	   abbrev string)
   prefix
   line))

(defsubst my-lint-layout-sql-create-table-error-data-type-size
  (string &optional prefix line)
  "Signal dtata type with size warning."
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, possibly unnecessary size spec: %s" string)
   prefix
   line))

(defun my-lint-layout-sql-check-create-table-col-part-lower
  (string &optional prefix line)
  "Check STRING keyword. E.g against lowercase and abbreviations."
  (my-lint-layout-with-case
    (dolist (re (list
		 my-lint-layout-sql-keywords-sql92-for-column
		 my-lint-layout-sql-keywords-sql-types))
      (let ((regexp (concat "\\(" re "\\)")))
	(when (string-match regexp string)
	  (my-lint-layout-sql-create-table-error-data-type-lower
	   (match-string 1 string)
	   prefix
	   (or line
	       (my-lint-layout-current-line-number))))
	(when (string-match
	       my-lint-layout-sql-keywords-sql-type-abbreviations
	       string)
	  (my-lint-layout-sql-create-table-error-data-type-abbrev
	   (match-string 0 string)
	   string
	   prefix
	   (or line
	       (my-lint-layout-current-line-number))))
	(when (let ((case-fold-search t))
		(string-match "\\<int.*(" string))
	  (my-lint-layout-sql-create-table-error-data-type-size
	   string
	   prefix
	   (or line
	       (my-lint-layout-current-line-number))))))))

(defun my-lint-layout-sql-check-create-table-col-error-type-close-paren
  (string &optional prefix line)
  "Signal error, extra space before paren in STRING. PREFIX, LINE."
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, extra space before closing paren: %s"
	   string)
   prefix
   (or line (my-lint-layout-current-line-number))))

(defun my-lint-layout-sql-check-create-table-col-error-unknown-word
  (string &optional prefix line)
  "Signal error, unknown keyword STRING. PREFIX, LINE."
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, unknown keyword: %s" string)
   prefix
   (or line (my-lint-layout-current-line-number))))

(defun my-lint-layout-sql-check-create-table-col-words
  (string &optional prefix line)
  "Split STRING against unknown keywords."
  (let ((clean
	 (replace-regexp-in-string
	  ;; Remove known keywords
	  my-lint-layout-sql-keywords-sql92-for-column
	  ""
	  (my-lint-layout-sql-clean-comments-string string))))
    (dolist (word (split-string clean))
      (when (string-match "^[a-z_]+$" word)
	(my-lint-layout-sql-check-create-table-col-error-unknown-word
	 word prefix line)))))

(defun my-lint-layout-sql-check-create-table-col-size
  (string &optional prefix line)
  "Check <type>(SIZE) defintioion from STRING."
  (when (string-match "\\([ \t]*\\)\\(([ ,0-9\t]+)+\\)" string)
    (let ((space (match-string 1 string))
	  (paren (match-string 2 string)))
      (unless (string= "" space)
	(my-lint-layout-message
	 (format "[sql] in CREATE TABLE, extra space before opening paren: %s"
		 string)
	 prefix line))
      (when (string-match "[ \t]+)" paren)
	(my-lint-layout-sql-check-create-table-col-error-type-close-paren
	 paren prefix line)))))

(defun my-lint-layout-sql-check-create-table-col-part-rest
  (string &optional prefix line)
  "In '<column name> <type <rest>', check the <rest> STRING."
  (my-lint-layout-sql-check-create-table-col-size
   string prefix line)
  (my-lint-layout-sql-check-create-table-col-words
   string prefix line))

(defun my-lint-layout-sql-check-create-table-non-column-name
  (str match &optional line prefix)
  (my-lint-layout-message
   (format "[sql] in CREATE TABLE, reserved keyword '%s' before column name: %s"
	   match str)
   prefix line))

(defsubst my-lint-layout-sql-create-table-adjust-line-number (str line)
  "STR Could match extra newline and point is at
wrong position. Match first non-LF up till last LF"
  (- line  (my-lint-layout-count-char-in-string ?\n str)))

(defun my-lint-layout-sql-create-table-adjust-line-maybe (string line)
  "Check STRING for embedded newlines at the end."
  ;;  The last line match will contain "col" all the way up till ";",
  ;;  So count lines correctly by reducing the matches newlines
  ;;
  ;;  CREATE TABLE table
  ;;  (
  ;;     ...
  ;;     col <TYPE>
  ;;  );
  (when (and (stringp string)
	     (string-match "[^\n][^\001]+\n\\([^\001]*\n\\)" string))
    (let ((str (match-string 1 string)))
      (setq line
	    (my-lint-layout-sql-create-table-adjust-line-number str line))))
  line)

(defun my-lint-layout-sql-check-create-table-primary-key
  (col str line prefix)
  "Check PRIMARY KEY line."
  ;;  error_id   INT   NOT NULL PRIMARY KEY AUTO_INCREMENT
  ;;  => the "error_" is probably redundant, simple 'id' will do.
  (cond
   ((string-match "^\\([^ \t\r\n]+\\)id" col)
    (my-lint-layout-message
     (format
      "[sql] in CREATE TABLE, probably unnecessary '%s' prefix in PK column: %s"
      (match-string 1 col) col)
     prefix))))

(defun my-lint-layout-sql-check-create-table-table-prefix
  (colname table line prefix)
  "Check PRIMARY KEY line."
  ;;  CREATE TABLE abc
  ;;  (
  ;;       abc_column
  ;;       ...
  (when (and table
	     (string-match
	      (format "^%s_" (regexp-quote table))
	      colname))
    (my-lint-layout-message
     (format
      "[sql] in CREATE TABLE, unnecessary table name '%s' prefix in column: %s"
      (match-string 0 colname) colname)
     prefix line)))

(defun my-lint-layout-sql-check-create-table-col-part
  (string &optional prefix line table)
  "Examine column defintion in STRING."
  (let ((re `,(concat
	       ;; PRIMARY KEY(user_id),
	       ;; col DECIMAL(1, 3)  PRIMARY KEY NOT NULL
	       ;;
	       ;; <col name> <type> <rest>
	       ;; 1          2 + 3
	       "\\([^ ,\t\r\n]+\\)"  ;; 1
	       "[ \t\r\n]+"
	       "\\(\\([^ ,(\t\r\n]+\\)\\(?:[ \t]*([^)\r\n]+[ \t]*)\\)?\\)" ;; 2
	       "\\(.*\\)")))         ;; 3
    (when (string-match re string)
      (let* ((match (match-string 0 string))
	     (name  (match-string 1 string))
	     (fulltype  (match-string 2 string))
	     (type  (match-string 3 string))
	     (rest  (match-string 4 string)))
	(cond
	 ((string-match "primary[ \t\r\n]+key" string)
	  (my-lint-layout-sql-check-create-table-primary-key
	   name string line prefix))
	 ((string-match
	   (concat "^[ \t\r\n]*"
		   my-lint-layout-sql-keywords-sql92-for-column)
	   match)
	  (my-lint-layout-sql-check-create-table-non-column-name
	   match (match-string 0 match) line prefix)))
	(my-lint-layout-sql-check-create-table-table-prefix
	 name table line prefix)
	(my-lint-layout-debug-message
	 "debug layout: CREATE A col part %d <<%s>>" line string)
	(setq line
	      (my-lint-layout-sql-create-table-adjust-line-maybe
	       string line))
	(my-lint-layout-debug-message
	 "debug layout: CREATE col part %d [[%s]]" line string)
	(my-lint-layout-sql-check-mixed-case
	 name
	 (format
	  "[sql] in CREATE TABLE, portability problem with mixed case: %s"
	  name)
	 prefix line)
	;; PRIMARY KEY (a, b)
	(unless (string-match "^[ \t\r\n]*primary[ \t\r\n]+key" string)
	  (my-lint-layout-sql-check-column-charset
	   match
	   (format "[sql] in CREATE TABLE, non-alphadigit characters: %s"
		   match)
	   prefix line)
	  (when fulltype
	    (my-lint-layout-sql-check-create-table-col-part-lower
	     fulltype prefix line))
	  (unless (string-match
		   my-lint-layout-sql-keywords-sql-types
		   type)
	    (my-lint-layout-message
	     (format
	      "[sql] in CREATE TABLE, non-standard or unknown data type: %s"
	      type)
	     prefix line))
	  (unless (string= "" rest)
	    (my-lint-layout-sql-check-create-table-col-part-rest
	     rest prefix line)))
	)))) ;; let

(defun my-lint-layout-sql-check-statement-create-tables-no-semicolon
  (&optional prefix line table)
  "Check cases, where semicolon is possibly missing.
An example:
    CREATE TABLE
    (

    )-!- missing semicolon"
;;;  (display-buffer (current-buffer)) ;; FIXME
  (while (re-search-forward "^[ \t]*)[ \t]*$" nil t)
    (my-lint-layout-message
     "[sql] in CREATE TABLE, possibly missing semicolon(;)"
     prefix
     (+ (or line 0) (my-lint-layout-current-line-number)))))

(defsubst my-lint-layout-sql-check-create-table-segment-primitive ()
  "Go to next segment primitive."
  (re-search-forward "\\([^,)]+)?\\)" nil t))

(defun my-lint-layout-sql-check-create-table-segment-forward ()
  "Go to next segment."
  ;;  Split until next colon(,) but this dowsnot necessarily work
  ;;  right. Examples:
  ;;
  ;;      col DECIMAL(2, 3),
  ;;      PRIMARY KEY (col1, col2),
  ;;      id INT(11) NOT NULL auto_increment PRIMARY KEY,
  ;;      size DECIMAL(11,2) NOT NULL,
  ;;
  (when (re-search-forward
	 ;; <col>  <type>(3,2),
	 "[ \t]*\\([^,]+\\(?:,[ \t\r\n]*[0-9]+[ \t\r\n]*)[^,]*\\)?\\)"
	 nil t)
    (let ((beg (match-beginning 0))
	  (end (match-end 0))
	  (str (match-string 0))
	  (m1b (match-beginning 1))
	  (m1e (match-end 1))
	  (str1 (match-string 1)))
      (when (and (not (looking-at ","))
		 (string-match "\\<primary[ \t\r\n]+key\\>" str)
		 (my-lint-layout-sql-check-create-table-segment-primitive))
	;;  Add a little more
	(let ((add (match-string 1)))
	  (setq m1e (point)
		str1 (concat str1 add))))
      (list
       beg end str m1b m1e str1))))

(defun my-lint-layout-sql-check-create-table-multiple-col-defs
  (&optional prefix line table)
  "Check multiple columen definitions at the same line."
  (let (info
	match
	curline)
    (while (setq info
		 (my-lint-layout-sql-check-create-table-segment-forward))
      (multiple-value-bind (beg end str m1b m1e match) info
	(setq curline (if line
			  (+ line (1- (my-lint-layout-current-line-number)))
			(my-lint-layout-current-line-number)))
	(my-lint-layout-debug-message
	 "debug layout: multiple-col-defs %d '%s'" curline match)
	;; Multiple, definitions, in line
	;; FIXME: Does not handle comments
	(when (looking-at ",.*[,;]")
	  (my-lint-layout-message
	   "[sql] in CREATE TABLE, possibly multiple columns(,) definitions"
	   prefix
	   curline))
	(my-lint-layout-sql-check-create-table-col-part
	 match prefix curline table)))))

(defun my-lint-layout-sql-check-statement-data-type-line-up
  (&optional prefix line table)
  "Check data type column for line up.

CREATE TABLE table
\(
    col   INTEGER,
    col   VARCHAR(80),
	  |
	  line up"
  (let ((re (concat
	     "[ \t\r\n]\\("
	     my-lint-layout-sql-keywords-sql-types
	     "\\)[ ,(\t\r\n]"
	     ))
	orig-col
	match
	curline
	col)
  (while (re-search-forward re nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (setq col (current-column)))
    (cond
     ((not orig-col)
      (setq orig-col col))
     ((not (eq col orig-col))
      (setq match (match-string 1))
      (setq curline (if line
			(+ line (1- (my-lint-layout-current-line-number)))
		      (my-lint-layout-current-line-number)))
      (my-lint-layout-message
       (format
	"[sql] In CREATE TABLE, data type possibly not lined-up with previous: %s"
	match)
       prefix curline))))))

(defun my-lint-layout-sql-check-statement-create-table-part
  (beg end &optional prefix line table)
  "Check CREATE TABLE content."
  (let ((string (buffer-substring beg end)))
    (with-temp-buffer
      (insert string)
;;;      (display-buffer (current-buffer)) ;; FIXME
      (my-lint-layout-sql-clean-comments-buffer)
      (dolist (function
	       '(my-lint-layout-sql-check-element-indentation
		 my-lint-layout-sql-check-statement-data-type-line-up
		 my-lint-layout-sql-check-statement-create-tables-no-semicolon
		 my-lint-layout-sql-check-create-table-multiple-col-defs))
	(goto-char (point-min))
	(my-lint-layout-debug-message
	 "debug layout: %s prefix %s line %s" function prefix (or line 0))
	(funcall function prefix line table)))))

(defsubst my-lint-layout-create-table-forward ()
  "Search CREATE TABLE forward.
The submatches are as follows: The point is at '!':

    CREATE TABLE name (<definition>) ;
    |----------- |---  |-----------  !
    1            2     3             Point"
  (re-search-forward
   `,(concat
      "^[ \t]*"
      "\\(create[ \t\r\n]+table\\)"
      "[ \t\r\n]+"
      "\\([^ \t\r\n]+\\)"
      "[ \t\r\n]*("
      ;;  Note, if there is *no* trailing ";", the command is incomplete.
      ;;  and not stanard SQL.
      ;;
      ;;  In that case we search statement end where
      ;;  closing paren ")" is its own line. Otherwise we cannot
      ;;  find statement end.
      ;;
      ;;  CREATE TABLE table
      ;;  (
      ;;      col DECIMAL(2, 3)
      ;;  )
       "\\([^;]+;\\|[^;]+\n+)[ \t]*\r?\n\\)")
   nil t))

(defun my-lint-layout-sql-create-table-error-unknown-keyword
  (str &optional prefix line)
  (my-lint-layout-message
   (format
    "[sql] in CREATE TABLE, unknown keyword: %s"
    str)
   prefix line))

(defun my-lint-layout-sql-check-statement-create-table-main
  (&optional prefix)
  "Check SQL syntax."
  (let (point
	line
	match
	keyword
	table
	content
	beg
	end)
    (while (my-lint-layout-create-table-forward)
      (setq point   (match-beginning 0)
	    match   (match-string 0)
	    keyword (match-string 1)
	    table   (match-string 2)
	    beg     (match-beginning 3)
	    end     (point)
	    line    (save-excursion
		      (goto-char point)
		      (my-lint-layout-current-line-number)))
      (when (string-match
	     my-lint-layout-sql-keywords-create-table-other
	     match)
	(my-lint-layout-sql-create-table-error-unknown-keyword
	 (match-string 1 match)
	 prefix line))
      (my-lint-layout-sql-check-all-uppercase
       keyword
       (format "[sql] in CREATE TABLE, keyword not uppercase: %s"
	       keyword)
       prefix line)
      (my-lint-layout-sql-check-mixed-case
       table
       (format "[sql] in CREATE TABLE, portability problem with mixed case: %s"
	       table)
       prefix line)
      (my-lint-layout-sql-check-charset
       table
       (format "[sql] in CREATE TABLE, non-alphadigit characters in %s" table)
       prefix line)
      (when (string-match "create.*table.*(" match)
	(my-lint-layout-message
	 "[sql] in CREATE TABLE, misplaced starting paren (expecting line-up)"
	 prefix line))
      (my-lint-layout-sql-check-statement-create-table-part
       beg end prefix line table))))

(defsubst my-lint-layout-sql-error-non-standard-comment (&optional line prefix str)
  "Signal error: non-standard SQL comment."
    (my-lint-layout-message
     (format "[sql] non-standard comment syntax%s"
	     (if str
		 (format ": %s" str)
	       ""))
     prefix
     (or line (my-lint-layout-current-line-number))))

(defsubst my-lint-layout-sql-check-comment-leading (&optional prefix)
  "Check SQL comments."
  (while (re-search-forward
	  "^[ \t]*\\(#\\|/[*]\\|//\\)"
	  nil t)
    (my-lint-layout-sql-error-non-standard-comment
     nil prefix (match-string 1))))

(defsubst my-lint-layout-sql-check-comment-trailing (&optional prefix)
  "Check SQL comments."
  (let (str)
    (while (re-search-forward
	    "\\(#+[^#\r\n]*\\|/[*].*[*]/\\)[ \t]*\r?\n"
	    nil t)
      (setq str (match-string 1))
      (save-excursion
	(goto-char (match-beginning 1))
	(my-lint-layout-sql-error-non-standard-comment nil prefix str)))))

(defun my-lint-layout-sql-check-comments (&optional prefix)
  "Check SQL comments from `current-point'. Save point."
  (let ((point (point)))
    (my-lint-layout-sql-check-comment-leading prefix)
    (goto-char point)
    (my-lint-layout-sql-check-comment-trailing prefix)
    (goto-char point)))

(defun my-lint-layout-sql-check-batch-all (&optional prefix)
  "Check SQL"
  (my-lint-layout-generic-run-list
   my-lint-layout-check-sql-functions prefix))

(defun my-lint-layout-sql-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-sql-check-batch-all'."
  (my-lint-layout-with-point-min
    (my-lint-layout-sql-check-batch-all)))

(defun my-lint-layout-sql-buffer-interactive (&optional prefix)
  "Run `my-lint-layout-sql-buffer'."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-sql-buffer prefix)))

;;; ............................................................. &css ...

(defsubst my-lint-layout-php-check-multiple-statements (msg &optional prefix)
  "Check multiple ';'."
  (when (looking-at ".+;.*;")
    (my-lint-layout-message msg prefix)))

(defun my-lint-layout-css-indent-level (&optional prefix)
  "Check indent."
  (let ((statement-p (when (looking-at "[ \t]*[a-z]")
		       (skip-chars-forward " \t")
		       t))
	(step my-lint-layout-generic-indent-step)
	col)
    (when (and statement-p
	       (setq col (current-column))
	       (not (zerop (mod col step))))
      (my-lint-layout-message
       (format "[css] body is not indented by %d at col %s" step col)
       prefix))))

(defun my-lint-layout-css-attribute (&optional prefix)
  "Check attribute."
  (when (and (looking-at "\\(.*[a-z]\\):[^{]+;")
	     ;;  a:hover
	     (not (string-match "\\<a" (match-string 1)))
	     (looking-at "\\([^ \t\r\n]+[a-z]:\\([^ \t\r\n]+\\)\\)"))
    (my-lint-layout-message
     (format "[css] no space between colon and attribute value: %s"
	     (match-string 1))
     prefix)))

(defun my-lint-layout-css-color (&optional prefix)
  "Check color-attribute."
  (let (str)
    (when (and (looking-at ".*\\<color:[ \t]*#\\([a-f0-9]+\\)")
	       (setq str (match-string 1))
	       (not (eq 6 (length str))))
      (my-lint-layout-message
       (format "[css] color is not complete 6 digit hex value: %s"str)
       prefix))))

(defun my-lint-layout-css-attribute-body-region (beg end &optional prefix)
  "Check in region BEG END that attributes values line up.

  color:        navy;
  background:   #FFFF;
  margin-left:  1px;"
  (let ((re   `,(concat
		 ;; margin-left: 8px;
		 "^[ \t]*"
		 "\\("                      ;1
		     "\\([^ ;\t\r\n]+\\)"   ;2
		     "\\([ \t]*\\)"         ;3
		     ":"
		     "\\([ \t]*\\)"         ;4
		     "\\([^ ;\t\r\n]+\\)"   ;5
		  "\\)"
		 ";"))
	orig-col
	str
	match
	attribute
	space1
	space2
	value
	val-beg
	match
	line
	point
	col)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward re end t)
	(setq str       (match-string 0)
	      match     (match-string 1)
	      attribute (match-string 2)
	      space1    (match-string 3)
	      space2    (match-string 4)
	      value     (match-string 5)
	      val-beg   (match-beginning 5)
	      point     (point))
	(goto-char val-beg)
	(setq col (current-column))
	(goto-char point)
	(my-lint-layout-debug-message
	 "%s" match)
	(cond
	 ((not orig-col)
	  (setq orig-col col))
	 ((not (eq col orig-col))
	  (my-lint-layout-message
	   "[css] attribute's value not lined-up with previous"
	   prefix
	   (my-lint-layout-current-line-number))))
	(when (string-match "font-size" match)
	  ;; font-size: .8em;
	  (when (string-match "[^0-9]\\(\\.[0-9]+\\)" match)
	    (my-lint-layout-message
	     (format
	      "[css] missing leading zero: %s"
	      match)
	     prefix))
	  (when (string-match "[0-9]\\(pt\\|px\\)" match)
	    (my-lint-layout-message
	     (format
	      "[css] possibly portability issue with '%s', em recommended: '%s'"
	      (match-string 1 match)
	      match)
	     prefix)))
	(when (string-match "font-family" str)
	  (unless (string-match
		   ",[ \t\r\n]*\\(?:\\(sans-\\)?serif\\|monospace\\|cursive\\)"
		   str)
	    (my-lint-layout-message
	     (format
	      "[css] portability; no last resort font sans-serif or serif listed: %s"
	      str)
	     prefix))
	  (unless (string-match my-lint-layout-css-web-safe-font-regexp str)
	    (let ((fonts (mapconcat
			  '(lambda (x)
			     (capitalize x))
			  my-lint-layout-css-web-safe-font-list
			  ",")))
	      (my-lint-layout-message
	       (format
		"[css] portability; no web safe font (e.g. %s) listed: %s"
		fonts str)
	       prefix))))
	(when (> (length space1) 0)
	  (my-lint-layout-message
	   (format "[css] extra space before colon: %s" match)
	   prefix))
	(my-lint-layout-css-indent-level prefix)
	(my-lint-layout-css-attribute prefix)
	(my-lint-layout-css-color prefix)
	(my-lint-layout-php-check-multiple-statements
	 "[css] multiple attribute definitions (only one expected)")))))

(defsubst my-lint-layout-css-body-end ()
  "Search body end poistion."
  (re-search-forward "\\}[ \t]*$" nil t))

(defun my-lint-layout-css-body (&optional beg prefix)
  "Check body, which starts at `current-point'."
  (or beg
      (setq beg (point)))
  (let ((end (my-lint-layout-css-body-end)))
    (when (and beg end)
      (my-lint-layout-debug-message
       "my-lint-layout-css-body: check region %d %d" beg end)
      (my-lint-layout-css-attribute-body-region beg end prefix))))

(defun my-lint-layout-css-skip-comment ()
  "Skip over comment if any."
  (when (looking-at "^[ \t]*/[*]")
    (search-forward "*/")))

(defun my-lint-layout-css-check-generic (&optional prefix)
  "Check Css"
  (let (str
	col
	len
	statement-p
	lines
	beg)
    (while (re-search-forward "{\\([ \t\r\n]*\\)" nil t)
      (setq beg      (match-beginning 0)
	    col      (current-column)
	    str      (match-string 1)
	    lines    (my-lint-layout-count-lines-in-string str))
      (my-lint-layout-css-skip-comment)
      (cond
       ((eq lines 0)
	(my-lint-layout-message
	 "[css] no newline after token '{'"
	 prefix))
      ((> lines 1)
	(my-lint-layout-message
	 (format "[css] extra %d empty lines after token '{'" (1- lines))
	 prefix)))
      (my-lint-layout-current-line-string)
      (when (looking-at ".*}")
	(my-lint-layout-message
	 "[css] inline {} body, expect line-up"
	 prefix))
      (when (looking-at ".*;\\([^ \t\r\n]\\)")
	(my-lint-layout-message
	 (format "[css] non-whitespace character after semicolon: %s" (match-string 1))
	 prefix))
      (when (eq col 0)
	(my-lint-layout-message
	 (format "[css] not indented (by %d)"
		 my-lint-layout-generic-indent-step)
	 prefix))
      ;;   background-color: #F8F8F8; border: 1px;
      (my-lint-layout-php-check-multiple-statements
       "[css] Multiple(;) attribute definitions, only one expected"
       prefix)
      (my-lint-layout-css-indent-level prefix)
      (my-lint-layout-css-body beg prefix))))

(defun my-lint-layout-css-comment-multiline-forward (&optional prefix)
  "Check multiline comments from point forward."
  (let (list)
    (while (setq list (my-lint-layout-generic-comment-multiline-stars))
      (multiple-value-bind (beg end point) list
	(my-lint-layout-generic-check-comment-multiline-stars
	 beg end prefix)))))

(defun my-lint-layout-css-comment-multiline-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-css-comment-multiline-forward'."
  (my-lint-layout-with-point-min
    (my-lint-layout-css-comment-multiline-forward prefix)))

(defun my-lint-layout-check-comment-javadoc-invalid (&optional prefix)
  "Check invalid Javadoc-style /** and @tag in comments."
  (let ((re1 (concat "^[ \t]*" (regexp-quote "/**")))
	(re2 "^[ \t]*[*][ \t]*@[a-z]"))
    (dolist (re (list re1 re2))
      (save-excursion
	(while (re-search-forward re nil t)
	  (my-lint-layout-message
	   (format "[css] possibly misplaced doc-block: %s"
		   (my-lint-layout-current-line-string))
	   prefix))))))

(defvar my-lint-layout-css-check-regexp-occur-variable
  '(my-lint-layout-generic-check-regexp-occur-line-up-style-list)
  "*List of occur variable names.")

(defun my-lint-layout-css-check-regexp-occur-main (&optional prefix)
  "Run all occur checks."
  (my-lint-layout-generic-run-occur-variable-list
   my-lint-layout-css-check-regexp-occur-variable prefix))

;; my-lint-layout-php-check-regexp-occur-variable prefix))

(defun my-lint-layout-css-check-batch-all (&optional prefix)
  "Check Css"
  (my-lint-layout-generic-run-list
   my-lint-layout-check-css-functions prefix))

(defun my-lint-layout-css-check-buffer (&optional prefix)
  "Check from `point-min'."
  (my-lint-layout-with-point-min
    (my-lint-layout-css-check-batch-all prefix)))

(defun my-lint-layout-css-check-buffer-interactive (&optional prefix)
  "Run `my-lint-layout-css-check-buffer'."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-css-check-buffer prefix)))

;;; ........................................................ &lined-up ...

(defun my-lint-layout-php-test-line-up-p (&optional col)
  "Check current `current-column' or COL for '=' and next line."
  (let ((treshold my-lint-layout-generic-assignment-line-up-treshold)
	(ret t)
	next)
    (or col
	(setq col (current-column)))
    (save-excursion
      (move-to-column col)
      (if (not (looking-at "="))
	  ;;  var =
	  ;;      "is too big to include in line";
	  ;;
	  ;; Accept this as is
	  'no-variable-follows
	(forward-line 1)
	(setq next (my-lint-layout-looking-at-assignment-column-p))))
    (when (and next
	       (or (null treshold)
		   (<= (abs (- col next)) treshold)))
      (setq ret (eq col next)))
    ret))

(defun my-lint-layout-php-check-line-up-assignment (&optional prefix)
  "Check that '=' line up."
  (let (str
	col
	len)
    (while (re-search-forward "^[ \t]*[$][a-zA-Z0-9_>. \t-]+=[^=\r\n]" nil t)
      (setq col (- (current-column) 2))
      (unless (my-lint-layout-php-test-line-up-p col)
	(my-lint-layout-message
	 (format "[code] assignment(=) at col %d possibly not lined-up"
		 col)
	 prefix))
      (forward-line 1))))

;;; ............................................................. &doc ...

(defsubst my-lint-layout-php-doc-string-narrowed-current-line-number ()
  "Return correct line number in narrowed doc-block."
  ;;  Because of narrowing, the first line is not 1, but 0.
  (+ line (1- (my-lint-layout-current-line-number))))

(defun my-lint-layout-generic-doc-string-test-function
  (str line &optional prefix data type)
  "Check docstring in STR, at LINE number. PREFIX for messages.
The DATA contains full function content as string."
  (let* ((class-p (my-lint-layout-with-save-point
		    (my-lint-layout-search-backward-class-p)))
	 (need-return-p
	  (and data
	       (string-match "^[ \t]*return\\>[ \t]*[^; \t\r\n]" data)))
	 (need-param-p
	  (and data
	       (string-match
		(concat
		 (my-lint-layout-generic-function-regexp)
		 "[ \t]*[^) \t\r\n]")
		data)))
	 (param  (string-match "@param" str))
	 (php-p  (my-lint-layout-code-php-p))
	 (access (string-match "@access" str))
	 return)
    (when (string-match "this[ \t]+\\(function\\|method\\)" str)
      (my-lint-layout-message
       (format "[doc] unnecessary wording: %s" (match-string 0 str))
       prefix line))
    (when (and php-p
	       class-p
	       (not access))
      (my-lint-layout-message
       "[doc] @access token not found"
       prefix line))
    (when (and need-param-p
	       (not param))
      (my-lint-layout-message
       "[doc] @param token not found"
       prefix line))
    (when (and param
	       (not need-param-p))
      (my-lint-layout-message
       "[doc] @param token is unnecessary"
       prefix line))
    (when (and return
	       (not need-return-p))
      (my-lint-layout-message
       "[doc] @return token is unnecessary"
       prefix line))
    (when (and need-return-p
	       (not (setq return (string-match "@return" str))))
      (my-lint-layout-message
       "[doc] @return token not found"
       prefix line))
    (if (and (and php-p access param)
	     (> access param))
	(my-lint-layout-message
	 "[doc] incorrect order. Should be @access..@param"
	 prefix line))
    (if (and (and php-p access return)
	     (> access return))
	(my-lint-layout-message
	 "[doc] incorrect order. Should be @access..@return"
	 prefix line))))

(defun my-lint-layout-php-doc-examine-content-function
  (str line &optional prefix data)
  "Examine content: function. Expects narrow to docstring.
STR is docstring at LINE number. PREFIX is for messages.
DATA is the full function content."
  (save-excursion
    ;;  * @param $var string
    (goto-char (point-min))
    (let (word
	  str)
      (while (re-search-forward
	      "[*][ \t]*@param[ \t]+\\([^ \t\r\n]+\\).*" nil t)
	(setq str  (match-string 0)
	      word (match-string 1))
	(unless (my-lint-layout-with-case
		  (string-match my-lint-layout-php-data-type-regexp
				word))
	  (let* ((case-p (string-match
			  my-lint-layout-php-data-type-regexp word))
		 (short-p (string-match
			   my-lint-layout-php-data-type-short-regexp word))
		 (short-case-p
		  (and short-p
		       (not (my-lint-layout-with-case
			      (string-match
			       my-lint-layout-php-data-type-short-regexp
			       word)))))
		 (match (and case-p
			     (match-string 1 word))))
	    ;; Check marked custom datatype
	    (cond
	     ((string-match "\\<entity" str)
	      (my-lint-layout-message
	       (format
		"@param announces custom datatype: %s" word)
	       prefix
	       (+ line (my-lint-layout-current-line-number))))
	     (t
	      (my-lint-layout-message
	       (format
		(concat
		 "@param announces unknown datatype"
		 (cond
		  (case-p
		   (format " (check spelling)" match))
		  ((and short-p
			short-case-p)
		   (format " (possibly abbreviated and incorrect case)" match))
		  (short-p
		   (format " (possibly abbreviated)" match))
		  (t
		   ""))
		 ": %s")
		word)
	       prefix
	       (+ line (my-lint-layout-current-line-number)))))))))))

(defun my-lint-layout-php-doc-string-test-var-class (str line &optional prefix)
  "Examine dostring: variable."
  (let ((access-p (string-match "@access" str))
	(var-p    (string-match "@var" str)))
    (unless access-p
      (my-lint-layout-message
       "[doc] @access token not found"
       prefix line))
    (unless var-p
      (my-lint-layout-message
       "[doc] @var token not found"
       prefix line))
    (when (and access-p
	       var-p
	       (> access-p var-p))
      (my-lint-layout-message
       "[doc] incorrect order. Should be @access..@var"
       prefix line))))

(defun my-lint-layout-php-doc-string-test-var-global (str line &optional prefix)
  "Examine dostring: variable."
  (when (string-match "[*][ \t]*\\(@[a-z][a-z][a-z].*\\)" str)
    (my-lint-layout-message
     (format "[doc] Possibly misplaced token: %s" (match-string 1))
     prefix line)))

(defun my-lint-layout-php-doc-string-test-class (str line &optional prefix)
  "Examine dostring: class."
  (unless (string-match "@package" str)
    (my-lint-layout-message
     "[doc] @package token not found"
     prefix line)))

(defun my-lint-layout-java-doc-string-test-class (str line &optional prefix)
  "Examine dostring: class."
  (unless (string-match "\\* @author" str)
    (my-lint-layout-message
     "[doc] @author token not found"
     prefix line))
  (unless (string-match "\\* @version" str)
    (my-lint-layout-message
     "[doc] @version token not found"
     prefix line))
  (unless (string-match "\\* @since" str)
    (my-lint-layout-message
     "[doc] @since token not found"
     prefix line)))

(defun my-lint-layout-php-doc-examine-content-other--test-doc-comment
  (line &optional type prefix)
  "Check doc-comment."
    (unless (looking-at "^[ \t]+[*]")
      (my-lint-layout-message
       "[doc] not a valid documentation comment"
       prefix
       (1+ line))))

(defun my-lint-layout-php-doc-examine-content-other--test-period
  (line &optional type prefix)
  "Check that line ends to a period."
  ;;  Complete sentence ends to period.
  (when (and (not (looking-at "^[ \t]+[*][ \t]*@")) ;; Std, not token line
	     (not (looking-at "^[ \t]+[*].*\\.")))
    (my-lint-layout-message
     (format
      "[doc] line is not a complete sentence ending to period(.)%s"
      (if (memq 'include type)
	  " (interpreted as require or include comment)"
	""))
     prefix
     (1+ line))))

(defun my-lint-layout-php-doc-examine-content-other--first-sentence
  (line &optional type prefix)
  "Check two words; that first line is a sentence."
  (when (and (not
	      (looking-at
	       (concat ".*"
		       my-lint-layout-generic-doc-1st-line-ignore-regexp)))
	     (not (looking-at
		   "^[ \t]+[*][ \t]*[^ \t\r\n]+[ \t][^ \t\r\n]+")))
    ;; Search at least two words. Ignore toplevel comments
    (when (and (not (memq 'file type))
	       (not (memq 'class type)))
      (my-lint-layout-message
       (format "[doc] line does not explain code that follows%s"
	       (if (memq 'include type)
		   " (interpreted as require or include comment)"
		 ""))
       prefix
       (1+ line)))))

(defun my-lint-layout-php-doc-examine-content-other--first-capital
  (line &optional type prefix)
  "Check first line capital letter."
  (my-lint-layout-with-case
    (when (and (not (looking-at "^[ \t]+[*][ \t]*@")) ;; Std, not token line
	       (not (looking-at "^[ \t]+[*][ \t]*[A-Z]")))
      (my-lint-layout-message
       (format "[doc] sentence does not start with capital letter%s"
	       (if (memq 'include type)
		   " (interpreted as require include comment)"
		 ""))
       prefix
       (1+ line)))))

(defun my-lint-layout-php-doc-examine-content-other--star-indent
  (line &optional prefix)
  "Check that words are indented by one or more spaces.

/**
 *Text
 *Text
 */"
    (my-lint-layout-with-case
      (when (and (looking-at "^[ \t]+[*]")
		 (not (looking-at "^[ \t]+[*][*/\r\n]"))
		 (looking-at "^[ \t]+[*][^ \t]"))
	(my-lint-layout-message
	 "[doc] near *-character; text is not indented with a space"
	 prefix
	 (1+ line)))))

(defun my-lint-layout-php-doc-examine-content-other--first-separator
  (line &optional type prefix)
  "Check that first line is seperated by one empty line.
/**
 *  First Line. Short description.
 *  <empty line>
 *  Long description.
 */"
  (my-lint-layout-with-case
    (unless (looking-at "^[ \t]*[*][ \t]*$\\|^[ \t]+[*]/")
      (my-lint-layout-message
       "[doc] no empty line after first line short description"
       prefix
       (+ 2 line))))
  (goto-char (point-min)))

(defun my-lint-layout-php-doc-examine-content-other--empty-line-tokens
  (line &optional type prefix)
  "Check empty line before @-tokens."
  (when (re-search-forward "^[ \t]*[*][ \t]@" nil t)
    (forward-line -1)
    (unless (looking-at "^[ \t]*[*][ \t]*$")
      (my-lint-layout-message
       "[doc] no empty line before starting @-token"
       prefix
       (+ line (my-lint-layout-current-line-number))))))

(defun my-lint-layout-php-doc-examine-content-other--indent-text
  (line &optional prefix)
  "Check proper indent."
  (when (my-lint-layout-doc-line-indent-p)
    (let ((text (match-string 2)))
      (unless (string= text text-indent)
	(my-lint-layout-message
	 "[doc] text indentation mismatch: lined-upnot same as above."
	 prefix
	 (my-lint-layout-php-doc-string-narrowed-current-line-number))))))

(defun my-lint-layout-php-doc-examine-content-other--indent-col-error
  (col line &optional prefix)
  "Write error:  *-character possibly not lined up at COL."
  (my-lint-layout-message
   (format "[doc] *-character does not start at column %d" col)
   prefix line))

(defun my-lint-layout-php-doc-examine-content-other--indent-col
  (col line &optional prefix)
  "Check that *-character is lined up at COL.
Write error at LINE with PREFIX."
  (let (point)
    (when (setq point (my-lint-layout-doc-line-indent-p))
      (my-lint-layout-with-save-point
	(goto-char point)
	(unless (eq (current-column) col)
	  (my-lint-layout-php-doc-examine-content-other--indent-col-error
	   col
	   (my-lint-layout-php-doc-string-narrowed-current-line-number)
	   prefix))))))

(defun my-lint-layout-php-doc-examine-content-other--doc-block-start-error
  (line &optional prefix string)
  "Write error: doc-block start contains extra characters."
  (my-lint-layout-message
   (format "[doc] /** contains extra characters%s" (or string ""))
   prefix line))

(defun my-lint-layout-php-doc-examine-content-other--doc-block-start
  (line &optional type prefix)
  "Check that /** is alone.

/**<no characters here>
 *
 */"
  (goto-char (line-beginning-position))
  (when (re-search-forward "[ \t]*/[*][*]" (line-end-position) t)
    (when (and (not (looking-at "[ \t\r\n]*$"))
	       (looking-at ".*"))
      (let ((str  (concat ": " (match-string 0))))
	(my-lint-layout-php-doc-examine-content-other--doc-block-start-error
	 (my-lint-layout-php-doc-string-narrowed-current-line-number)
	 prefix
	 str)))))

(defun my-lint-layout-php-doc-examine-content-other--all-lines
  (line &optional type prefix)
  "Check until `point-min' all lines."
  (let (text-indent
	col-indent)
    (while (not (eobp))
      ;; *-line; Which column is the indent
      (when (and (not col-indent)
		 (setq point (my-lint-layout-doc-line-startp-p)))
	(goto-char (1+ point))
	(setq col-indent (current-column))
	(my-lint-layout-php-doc-examine-content-other--doc-block-start
	 line type prefix))
      ;; *-line; Initial values from first line that contains text
      (when col-indent
	(when (and (not text-indent)
		   (my-lint-layout-doc-line-indent-p)
		   (not (string= (match-string 2) "")))
	  (setq text-indent (match-string 2)))
	(my-lint-layout-php-doc-examine-content-other--indent-col
	 col-indent line prefix)
	;; (my-lint-layout-php-doc-examine-content-other--indent-text)
	(my-lint-layout-php-doc-examine-content-other--star-indent line prefix))
      (forward-line 1))))

(defun my-lint-layout-php-doc-examine-content-other
  (str line type &optional prefix)
  "Examine docstring."
  (save-excursion
    (goto-char (point-min))
    (my-lint-layout-with-save-point
      (my-lint-layout-php-doc-examine-content-other--all-lines
       line type prefix))
    (forward-line 1)
    (my-lint-layout-php-doc-examine-content-other--test-doc-comment
     line type prefix)
    (my-lint-layout-php-doc-examine-content-other--test-period
     line type prefix)
    (unless (memq 'file type)
      (my-lint-layout-php-doc-examine-content-other--first-sentence
       line type prefix)
      (my-lint-layout-php-doc-examine-content-other--first-capital
       line type prefix))
    (unless (or (memq 'include type)
		(memq 'class type)
		(memq 'var-global type))
      (forward-line 1)
      (my-lint-layout-php-doc-examine-content-other--first-separator
       line type prefix)
      (forward-line 1)
      (my-lint-layout-php-doc-examine-content-other--empty-line-tokens
       line type prefix))))

(defun my-lint-layout-php-doc-examine-typeof (str)
  "Examine what type of docstring."
  (let (type)
    (if (my-lint-layout-type-class-string-p str)
	(push 'class type))
    (if (my-lint-layout-type-function-string-p str)
	(push 'function type))
    (if (my-lint-layout-type-include-string-p str)
	(push 'include type))
    (if (my-lint-layout-type-class-variable-dollar-string-p str)
	(push 'var type))
    (if (my-lint-layout-type-variable-string-p str)
	(push 'var-global type))
    type))

(defun my-lint-layout-java-doc-examine-typeof (str)
  "Examine what type of docstring."
  (let (type)
    (if (my-lint-layout-type-class-string-p str)
	(push 'class type))
    (if (my-lint-layout-type-function-string-p str)
	(push 'function type))
    (if (my-lint-layout-type-import-string-p str)
     	(push 'include type))
    ;; FIXME
    ;; (if (my-lint-layout-type-class-variable-dollar-string-p str)
    ;; 	(push 'var type))
    (if (my-lint-layout-type-variable-string-p str)
	(push 'var-global type))
    type))

(defun my-lint-layout-generic-doc-examine-typeof (str)
  "Examine what type of docstring."
  (cond
   ((eq 'php-mode (my-lint-layout-code-type-p))
    (my-lint-layout-php-doc-examine-typeof str))
   ((eq 'java-mode (my-lint-layout-code-type-p))
    (my-lint-layout-java-doc-examine-typeof str))))

(defun my-lint-layout-generic-function-end (&optional column indent)
  "Return end of function. Optional COLUMN and INDENT string.
Search for closing brace located at the same COLUMN.

Input:
  COLUMN  optional number, defaults to `current-column'.
  INDENT  optional string, the indent level as string.

Return:
  point"
  (let (indent-string
	last-col
	last
	point)
    (or column
	(setq column (current-column)))
    ;; FIXME: convert `indent' from tabs to spaces.
    (setq indent-string (make-string column ?\ ))
    (or
     ;; Search same indentation for closing brace
     (and indent
	  (re-search-forward (concat "^" indent "}") nil t)
	  (setq point (point)))
     (and column
	  ;; Or search for brace at the same column
	  (while (and (not point)
		      (search-forward "}" nil t)
		      (not (eobp)))
	    (cond
	     ((= (1- (current-column)) column)
	      (setq point (point)))
	     ((and last-col
		   ;; This is back-indent already. Suppose previous
		   ;; one was the last "known closing indent"
		   (< (1- (current-column)) column))
	      (setq point last)))
	    (setq last (point)
		  last-col (1- (current-column))))))
    point))

(defun my-lint-layout-generic-function-region-at-point ()
  "Return function '(beg end) points with indentation.
Point must be at the beginning of function definition line."
  (save-excursion
    (goto-char (line-beginning-position))
    (let ((re-beg
	  (cond
	   ((eq 'php-mode (my-lint-layout-code-type-p))
	    my-lint-layout-php-function-regexp)
	   ((eq 'java-mode (my-lint-layout-code-type-p))
	    my-lint-layout-java-function-regexp)))
	  re-end
	  indent
	  col
	  beg
	  end)
    (when (and re-beg
	       (looking-at re-beg))
      (setq beg (point))
      ;; This can be spaces+tabs, so canonicalize
      (setq indent (match-string 1))
      (goto-char (match-end 1))
      (setq col (current-column))
      (when (setq end (my-lint-layout-generic-function-end col indent))
	(list beg end))))))

(defun my-lint-layout-generic-function-string-at-point ()
  "Return function string if any at point."
  (multiple-value-bind (beg end)
      (my-lint-layout-generic-function-region-at-point)
    (when beg
      (buffer-substring beg end))))

(defun my-lint-layout-php-doc-examine-main (beg end type line &optional prefix)
  "Examine docstring
  ;;    /**
  ;;     * Short description
  ;;     *
  ;;     * Full description .....
  ;;     */"
  (save-restriction
    (save-excursion
      (let (data)
	(goto-char end)
	(unless (zerop (skip-chars-forward " \t\r\n"))
	  (goto-char (line-beginning-position))
	  (setq data (my-lint-layout-generic-function-string-at-point)))
	(narrow-to-region beg end)
	(let ((str (buffer-string)))
	  (cond
	   ((memq 'var type)
	    (my-lint-layout-php-doc-string-test-var-class str line prefix))
	   ((memq 'var-global type)
	    (my-lint-layout-php-doc-string-test-var-global str line prefix))
	   ((memq 'class type)
	    (my-lint-layout-php-doc-string-test-class str line prefix))
	   ((memq 'function type)
	    (my-lint-layout-generic-doc-string-test-function
	     str line prefix data)
	    (my-lint-layout-php-doc-examine-content-function
	     str line prefix data)))
	  (my-lint-layout-php-doc-examine-content-other
	   str line type prefix))))))

(defun my-lint-layout-java-doc-examine-main (beg end type line &optional prefix)
  "Examine docstring
  ;;    /**
  ;;     * Short description
  ;;     *
  ;;     * Full description .....
  ;;     */"
  (save-restriction
    (save-excursion
      (let (data)
	(goto-char end)
	(unless (zerop (skip-chars-forward " \t\r\n"))
	  (goto-char (line-beginning-position))
	  (setq data (my-lint-layout-generic-function-string-at-point)))
	(narrow-to-region beg end)
	(let ((str (buffer-string)))
	  (cond
	   ((memq 'var type)
	    (my-lint-layout-php-doc-string-test-var-class str line prefix))
	   ((memq 'var-global type)
	    (my-lint-layout-php-doc-string-test-var-global str line prefix))
	   ((memq 'class type)
	    (my-lint-layout-java-doc-string-test-class str line prefix))
	   ((memq 'function type)
	    (my-lint-layout-generic-doc-string-test-function
	     str line prefix data)))
	  (my-lint-layout-php-doc-examine-content-other
	   str line type prefix))))))

(defun my-lint-layout-php-check-doc--test-empty-line-above (&optional message)
  "Check empty line before doc-block."
  (my-lint-layout-with-save-point
    (forward-line -1)
    (when (and (not (bobp))
	       (not (looking-at "^[ \t]*[{<]\\|^[ \t\r]*$")))
      ;; private $var;
      ;; /**
      ;;  * Documentation
      ;;  */
      (my-lint-layout-message
       "[newline] no empty line before documentation block."
       prefix))))

(defun my-lint-layout-generic-check-doc-main (&optional prefix)
  "Check doc-block.

/**
 * Short description.
 *
 * Long description.
 *
 * @token
 * @token
 */
 <code>"
  (let (point
	next-line-valid-p
	toplevel-p
	valid-p
	str
	beg
	end
	type)
    (while (my-lint-layout-search-forward-doc-beginning)
      (setq point       (point)
	    beg         (line-beginning-position)
	    top-level-p (my-lint-layout-with-save-point
			  (my-lint-layout-top-level-p)))
      (my-lint-layout-php-check-doc--test-empty-line-above)
      (when (save-excursion
	      (forward-line 1)
	      (setq next-line-valid-p (my-lint-layout-looking-at-doc-p))
	      (setq end (my-lint-layout-search-forward-doc-end))
	      (skip-chars-forward " \t\r\n")
	      (setq valid-p
		    (or top-level-p
			(my-lint-layout-looking-at-doc-end-valid-p)
			;; File level comment
			(string-match "Copyright\\|License"
				      (buffer-substring point end))))
	      (setq type
		    (my-lint-layout-generic-doc-examine-typeof
		     (my-lint-layout-current-line-string)))
	      (if (and (not type)
		       valid-p)
		  (setq type '(file)))
	      end)
	(setq str (buffer-substring beg end))
	(unless next-line-valid-p
	  (my-lint-layout-message
	   "[doc] format layout error"
	   prefix))
	(cond
	 ;; (my-lint-layout-doc-package-string-p str)) ;Skip
	 ;; (my-lint-layout-doc-var-string-p str)) ;Skip
	 ((not valid-p)
	  (my-lint-layout-message
	   (concat
	    "[doc] possibly misplaced. "
	    "Expected class, function, variable, require or include")
	   prefix))
	 (t
	  (let ((top-level-p (my-lint-layout-doc-package-string-p str)))
	    (my-lint-layout-java-doc-examine-main
	     beg
	     end
	     (if top-level-p
		 '(class)
	       type)
	     (my-lint-layout-current-line-number)
	     prefix))))))))

;;; ............................................................ &mode ...

(defvar my-lint-output-mode-hook nil
  "*Hook run when `my-lint-output-mode' is called.")

(defvar my-lint-output-mode-map (make-sparse-keymap)
  "*Keymap")

;; font-lock-constant-face
(defvar my-lint-output-mode-font-lock-keywords
  (list
   (list
    (list
     "^\\([^:]+\\):+\\([0-9]+\\)"
     '(1 'font-lock-function-name-face)  ;; filename
     '(2 'font-lock-constant-face))      ;; line
;;;    (list
;;;     "^\\(To view errors and warnings, look at\\) +\\(.+\\)"
;;;     '(1 'font-lock-builtin-face)
;;;     '(2 'font-lock-function-name-face))
;;;    (list
;;;     "^Converting +\\([^ \t\rn\n]+\\).*Procedural"
;;;     1 'font-lock-function-name-face)
;;;    (list
;;;     "[0-9]+ +seconds"
;;;     0 'font-lock-constant-face)))
    ))
  "*Fontification.")

(defun my-lint-output-mode-error-at-point-p ()
  "Check current line for errors."
  (if (looking-at "^\\([^:]+\\):+\\([0-9]+\\)")
      (list (match-string 1)
	    (string-to-int (match-string 2)))))

(defun my-lint-output-mode-error-info ()
  "Return filename and line at point."
  (save-excursion
    (goto-char (line-beginning-position))
    (my-lint-output-mode-error-at-point-p)))

(defsubst my-lint-output-mode-find-buffer (name)
  "Return bufer pointer for buffer NAME."
  (or (get-buffer name)
      (find-buffer-visiting name)))

(defun my-lint-output-mode-goto-line-key ()
  "Go to file or dir at point."
  (interactive)
  (multiple-value-bind (file line)
      (my-lint-output-mode-error-info)
    (if (not file)
	(message "no file information found at current line.")
      (let ((buffer (my-lint-output-mode-find-buffer file)))
	(if (not buffer)
	    (message "can't find buffer for '%s'" file)
	  (pop-to-buffer buffer)
	  ;; (find-file-other-window file)))
	  (if line
	      (goto-line line)))))))

(defun my-lint-output-mode-goto-line-mouse (event)
  (interactive "e")
  (let* ((epoint (event-start event))
	 (win    (or (and epoint
			  (nth 0 epoint))
		     (get-buffer-window (current-buffer))))
	 (point  (or (and epoint
			  (nth 1 epoint))
		     (point))))
    (select-window win)
    (goto-char point)
    (my-lint-output-mode-goto-line-key)))

(defun my-lint-output-mode-map-define ()
  "Define keymap."
  (define-key my-lint-output-mode-map
    "\C-c\C-c"
    'my-lint-output-mode-goto-line-key)
  (define-key my-lint-output-mode-map
    "\C-m" 'my-lint-output-mode-goto-line-key)
  (define-key my-lint-output-mode-map
    [(mouse-2)]'my-lint-output-mode-goto-line-mouse)
  ;; Set up the menu-bar
  (define-key my-lint-output-mode-map [menu-bar](make-sparse-keymap))
  (define-key my-lint-output-mode-map [menu-bar lint-layout]
    (cons "Lint" (make-sparse-keymap "Lint")))
  ;; '("----" . nil)
  ;; Bottom up order
  (define-key my-lint-output-mode-map
    [menu-bar lint-layout my-lint-output-mode-goto-line-mouse]
    '("Goto error at point" . my-lint-output-mode-goto-line-mouse)))

;;;###autoload
(define-derived-mode my-lint-output-mode fundamental-mode "Lint"
  "Major mode for Lint output.
Runs `my-lint-output-mode-hook'."
  (setq buffer-read-only t)
  (my-lint-output-mode-map-define)
  (setq font-lock-defaults
	my-lint-output-mode-font-lock-keywords)
  (if (or font-lock-mode
	  global-font-lock-mode)
      (font-lock-fontify-buffer)))

;;; ................................................ &java-interactive ...

(defun my-lint-layout-java-check-all-tests (&optional prefix)
  "Run `my-lint-layout-check-java-generic-functions'."
  (my-lint-layout-generic-run-list
   my-lint-layout-check-java-generic-functions prefix))

(defun my-lint-layout-java-check-code-run (&optional point prefix)
  (my-lint-layout-generic-run-list
   (append
    my-lint-layout-check-java-code-functions
    my-lint-layout-check-generic-functions)
   prefix
   point))

(defun my-lint-layout-java-check-code-interactive (&optional point prefix)
  "Run code checks from current POINT forward.
This includes:
  `my-lint-layout-check-java-code-functions'
  `my-lint-layout-check-generic-functions'"
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-java-check-code-run) point prefix))

(defun my-lint-layout-java-check-javadoc-run (&optional point prefix)
  (my-lint-layout-generic-run-list
   my-lint-layout-check-java-doc-functions
   prefix
   point))

(defun my-lint-layout-java-check-javadoc-interactive
  (&optional point prefix erase)
  "Run `my-lint-layout-check-java-doc-functions' from current POINT forward."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-java-check-javadoc-run point prefix)))

(defun my-lint-layout-java-check-all-interactive (&optional point prefix)
  "Run All JAVA checks."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-java-check-all-tests prefix)))

;;; ................................................. &php-interactive ...

(defun my-lint-layout-php-check-all-tests (&optional prefix)
  "Run `my-lint-layout-check-php-generic-functions'."
  (my-lint-layout-generic-run-list
   my-lint-layout-check-php-generic-functions prefix))

(defun my-lint-layout-php-check-code-run (&optional point prefix)
  (my-lint-layout-generic-run-list
   (append
    my-lint-layout-check-php-code-functions
    my-lint-layout-check-generic-functions)
   prefix
   point))

(defun my-lint-layout-php-check-code-interactive (&optional point prefix)
  "Run code checks from current POINT forward.
This includes:
  `my-lint-layout-check-php-code-functions'
  `my-lint-layout-check-generic-functions'"
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-php-check-code-run) point prefix))

(defun my-lint-layout-php-check-phpdoc-run (&optional point prefix)
  (my-lint-layout-generic-run-list
   my-lint-layout-check-php-doc-functions
   prefix
   point))

(defun my-lint-layout-php-check-phpdoc-interactive
  (&optional point prefix erase)
  "Run `my-lint-layout-check-php-doc-functions' from current POINT forward."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-php-check-phpdoc-run point prefix)))

(defun my-lint-layout-php-check-all-interactive (&optional point prefix)
  "Run All PHP checks."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-php-check-all-tests prefix)))

;;; ............................................. &generic-interactive ...

(defun my-lint-layout-check-generic-buffer
  (&optional prefix verb)
  "Run checks. PREFIX is displayed at the beginning of line. VERB.
According to file extension: *.php, *.css, *.php."
  (interactive
   (list nil 'verbose))
  (let ((name (buffer-name)))
    (or prefix
	(setq prefix name))
    (my-lint-layout-code-type-set-local-variable) ; Makes things faster
    (cond
     ((eq 'php-mode (my-lint-layout-code-type-p))
      (my-lint-layout-php-check-all-interactive (point-min) prefix))
     ((eq 'java-mode (my-lint-layout-code-type-p))
      (my-lint-layout-java-check-all-interactive (point-min) prefix))
     ((eq 'css-mode (my-lint-layout-code-type-p))
      (my-lint-layout-css-check-buffer-interactive prefix))
     ((eq 'sql-mode (my-lint-layout-code-type-p))
      (my-lint-layout-sql-buffer-interactive prefix))
     (t
      (if verb
	  (message "no checks defined for: %s" prefix))))))

(defun my-lint-layout-check-generic-file (file &optional verb)
  "Run check on FILE. VERB."
  (interactive
   (list (read-file-name "File to check:")
	 'verbose))
  (let (find-file-hooks)
    (with-current-buffer (find-file file)
      (goto-char (point-min))
      (my-lint-layout-check-generic-buffer
       file
       verb))))

(defun my-lint-layout-check-generic-directory
  (dir &optional verb)
  "Run check in DIR for files. VERB.
See `my-lint-layout-check-generic-buffer'"
  (interactive
   (list (read-directory-name "Directory to check: ")
	 'verb))
  (dolist ((file (directory-files dir 'fullpath)))
    (when (and (not (file-directory-p file))
	       (file-exists-p file))
      (my-lint-layout-check-generic-file file verb))))

;;; ........................................................... &batch ...

(defun my-lint-layout-princ-results ()
  "Write results."
  (my-lint-layout-with-result-buffer
    (my-lint-layout-result-sort-lines)
    (my-lint-layout-debug-message
     "debug layout: Batch results %s %d"
     (buffer-name)
     (point-max))
    ;; to stderr. Hm.
    (unless (eq (point-min) (point-max))
      (princ (buffer-string)))))

(defun my-lint-layout-check-file-list (list function-list)
  "Check LIST of files with FUNCTION-LIST."
  (if (and list
	   (not (listp list)))
      (setq list (list list)))
  (if (and function-list
	   (not (listp function-list)))
      (setq function-list (list function-list)))
  (dolist (file list)
    (my-lint-layout-debug-message
     "debug layout: Batch running %s %s"
     (file-exists-p file)
     file)
    (if (not (file-exists-p file))
	(message "WARN: No such file '%s'" file)
      (let (find-file-hooks)
	(with-temp-buffer
	  (my-lint-layout-code-type-set-local-variable)
	  (insert-file-contents file)
	  (my-lint-layout-generic-run-list
	   function-list
	   file
	   (point-min)))))))

(defun my-lint-layout-check-generic-file (file)
  "Run checks according to file type over FILE.
The type of file is determined from file extension.
The `my-lint-layout-buffer-name' is not emptied nor displayed."
  (my-lint-layout-debug-message "debug layout: Generic file: %s" file)
  (cond
   ((string-match "\\.php$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-php-check-all-tests))
   ((string-match "\\.java$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-java-check-all-tests))
   ((string-match "\\.css$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-css-check-batch-all))
   ((string-match "\\.sql$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-sql-check-batch-all))
   (t
    (message "[WARN] No checks defined for file: %s" file))))

(defun my-lint-layout-check-batch-file-list (files &optional function)
  "Check FILES with FUNCTION (or list of)."
  (let ((debug-on-error t)
	debug-ignored-errors
	(default-directory default-directory)
	(dir default-directory))
    (if (and function
	     (not (listp function)))
	(setq function (list function)))
    (my-lint-layout-debug-message
     "debug layout: Batch cmdline %s; files %s" function files)
    (my-lint-layout-check-file-list files function)))

(when nil ;; For testing; an example
  (my-lint-layout-check-batch-file-list
   '("~/tmp/Example.java")
   '(my-lint-layout-java-check-all-tests)))

;; Selectively run test for list of files
(defun my-lint-layout-check-batch-command-line (&optional function)
  "Run FUNCTION (or list of) over files on command line.
See:
  my-lint-layout-check-whitespace
  my-lint-layout-php-check-all-tests
  my-lint-layout-java-check-all-tests"
  (my-lint-layout-check-batch-file-list
   command-line-args-left function)
  (my-lint-layout-princ-results))

(defun my-lint-layout-check-batch-generic-command-line ()
  "Run correct check for each type of file on command line."
  (let ((debug-on-error t))
    (dolist (file command-line-args-left)
      (my-lint-layout-check-generic-file file))
    (my-lint-layout-princ-results)))

;; End of file
