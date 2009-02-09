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
;;	o   Maximum code column 80
;;	o   Extra whitespace at end of lines (spaces and tabs)
;;	o   Brace placement (lined-up; no K&R support yet)
;;	o   Indentation multiple of 4.
;;	o   terminating semicolon checks: no loose "semicolons ;"
;;
;;	Some of the SQL checks include:
;;
;;	o   FIXME: todo
;;
;;	Some of the CSS checks include:
;;
;;	o   FIXME: todo
;;
;;	Some of the PHP checks include:
;;
;;	o   The use and definition of function or methods:
;;
;;	    function name () // in definition <neme> surrounding space
;;	    {
;;		call( $param, $param); // leading space in muti-arg calls
;;		call(aram);	       // single arg call
;;	    }
;;
;;	o   Readable keywords `and' and `or' preferred over
;;	    traditional && and ||.
;;	o   Literal keywords in lowercase: false, true, null
;;	o   Multiline concat dot-operator(.) could be written using
;;	    <<<HERE documents.
;;	o   Instead echo(), print() is suggested due to function to exist
;;	    in all programming languages.
;;	o   Instead of date(), POSIX standard strftime() preferred.
;;
;; User callable functions (M-x):
;;
;;      Normally functions start scanning from current point foward, unless
;;      "buffer" is mentioned:
;;
;;	    ;; Decides correct test set for *.css, *.php, *.sql file
;;	    my-lint-layout-check-generic-interactive
;;
;;	    my-lint-layout-php-check-all-interactive
;;	    my-lint-layout-php-check-phpdoc-interactive
;;
;;	    my-lint-layout-check-whitespace-buffer-interactive
;;	    my-lint-layout-check-line-length-buffer-interactive
;;	    my-lint-layout-check-eof-marker-interactive
;;
;;	    my-lint-layout-css-check-buffer-interactive
;;	    my-lint-layout-sql-buffer-interactive
;;
;;	Look at results in `my-layout-changelog-check-main' buffer which
;;	by default is `my-lint-layout-buffer-name'.
;;
;; Batch command line usage
;;
;;	This lisp library canbe called from command line with list of files
;;	to check:
;;
;;	    emacs -Q -q -l lint-layout.el -f

(require 'regexp-opt)

(eval-when-compile
  (require 'cl))

(defvar my-lint-layout-debug nil
  "Non-nil to turn on debug messages.")

(defconst my-lint-layout-buffer-name "*Layout checks*"
  "*Buffer name for results.")

(defconst my-lint-layout-generic-line-length-max 80
  "*Maximum line length.")

(defconst my-lint-layout-generic-indent-step 4
  "*Indent step.")

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
     "catch")
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
               "[ \t]+\\)[^(]*("
   "\\).*"
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
   "\\(\\<"
   (mapconcat
    'concat
    '("ereg"
      "array_[a-z_]+"
      "isset"
      "empty"
      "array"
      "date"
      "strftime"
      "header"
      "preg_[a-z]+"
      "is_[a-z]+"
      "mysql_[a-z_]+")
    "\\|")
   "\\)\\>")
  "Typical PHP functions.")

(defconst my-lint-layout-php-function-call-keywords-list
  (list my-lint-layout-php-function-call-keywords-no-paren
	my-lint-layout-php-function-call-keywords-generic)
  "PHP keyword list")

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

(defconst my-lint-layout-php-doc-location-regexp
   (concat
    ;; Prefix <?php
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
  "Location, where PHPDoc blocks should exist.")

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

(defconst my-lint-layout-php-brace-and-code-regexp
  "[}][ \t]*\r?\n[ \t]*\\([^{} \t\r\n]+\\)"
  "Match brace end } followed by immediate code.")

(defvar my-lint-layout-check-generic-functions
  '(my-lint-layout-check-whitespace
    my-lint-layout-check-line-length)
  "*List of generic lint functions.")

(defvar my-lint-layout-check-php-code-functions
  '(my-lint-layout-generic-class-count
    my-lint-layout-generic-xml-tags-check-main
    my-lint-layout-php-check-xml-tags-lazy
    my-lint-layout-php-check-multiple-print
    my-lint-layout-php-check-statement-end
    my-lint-layout-php-check-statement-start
    my-lint-layout-php-check-statement-start-2
    my-lint-layout-php-check-comment-statements
    my-lint-layout-php-check-control-statements
    my-lint-layout-php-check-block-end-and-code
    my-lint-layout-php-check-line-up-assignment
    my-lint-layout-php-check-brace-extra-newline
    my-lint-layout-php-check-regexp-occur-main
    my-lint-layout-php-class-check-variables
    my-lint-layout-php-check-multiline-print
    my-lint-layout-php-check-multiline-sql
    my-lint-layout-php-check-words
    my-lint-layout-php-check-keywords-main
    my-lint-layout-check-whitespace
    my-lint-layout-check-eof-marker
    ;; my-lint-layout-check-line-length
    )
  "*List of PHP code check functions")

(defvar my-lint-layout-check-php-doc-functions
  '(my-lint-layout-php-check-doc-missing
    my-lint-layout-php-check-doc-main)
  "*List of functions for PHPDoc.")

(defvar my-lint-layout-check-php-generic-functions
  (append my-lint-layout-check-php-doc-functions
	  my-lint-layout-check-php-code-functions)
  "List of all PHP check functions.")

(defvar my-lint-layout-check-sql-functions
  '(my-lint-layout-sql-check-create-table
    my-lint-layout-sql-check-keywords)
  "*List of functions for SQL.")

(defvar my-lint-layout-check-css-functions
  '(my-lint-layout-check-comment-javadoc-invalid
    my-lint-layout-css-check-generic)
  "*List of functions for CSS.")

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

(put 'my-lint-layout-point-min 'lisp-indent-function 0)
(put 'my-lint-layout-point-min 'edebug-form-spec '(body))
(defmacro my-lint-layout-point-min (&rest body)
  "Run BODY with from `point-min'. Point is preserved."
  `(save-excursion
    (goto-char (point-min))
    ,@body))

(put 'my-lint-layout-save-point 'lisp-indent-function 0)
(put 'my-lint-layout-save-point 'edebug-form-spec '(body))
(defmacro my-lint-layout-save-point (&rest body)
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

(defsubst my-lint-layout-paragraph-end-point ()
  "Return empty line or `point-max'."
  (save-excursion
    (or (re-search-forward "^[ \t]*$" nil t)
	(point-max))))

(defun my-lint-layout-current-line-number ()
  "Return line number. Lines are counted from 1..x"
  ;;  - always use line beginning as reference
  ;;  - The count-lines returns 0 for 1st line, therefore 1+
  (let ((beg (line-beginning-position))
	(i 1))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "\n" beg t)
	(incf i)))
    i))

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
  "Check @package phpdoc"
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

(defsubst my-lint-layout-php-search-forward-function-beginning ()
  (re-search-forward my-lint-layout-php-function-regexp nil t))

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

(defsubst my-lint-layout-search-forward-variable-dollar-beginning (&optional max)
  "Search variable start, up till optional MAX point."
  (and (re-search-forward
	`,(concat
	   "^[ \t]*"
	   "\\(?:"
	       my-lint-layout-generic-access-modifier-regexp
	       "[ \t]+"
	       "\\|\\<var\\>[ \t]+"
	       "\\)?"
	   "\\$_*[a-zA-Z][ \t]*[;=]")
	   max t)
       (match-end 0)))

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

(defsubst my-lint-layout-type-function-string-p (str)
  (string-match
   my-lint-layout-php-function-regexp
   str))

(defsubst my-lint-layout-type-include-string-p (str)
  (string-match "^[ \t]*require\\|include" str))

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

(defsubst my-lint-layout-string-comment-p (str)
  "Check if STR looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\)" str))

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

;; FIXME: comment-type 'sigle 'multi
(defsubst my-lint-layout-looking-at-comment-start-p ()
  "If `looking-at' at comment start."
  (looking-at "^[ \t]*\\(/?[*]\\|//\\)"))

(defsubst my-lint-layout-looking-at-comment-end-p ()
  "If `looking-at' at comment end."
  (looking-at "^[ \t]*\\(//\\|[*]/\\)"))

;; FIXME: use string-match
(defsubst my-lint-layout-looking-at-comment-point-p ()
  "If `looking-at' at comment."
  (my-lint-layout-save-point
    (goto-char (line-beginning-position))
    (looking-at "^[ \t]*\\(/?[*]\\|//\\|[*]/\\)")))

(defsubst my-lint-layout-looking-at-comment-line-p ()
  "Check if line looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\)"
		(my-lint-layout-current-line-string)))

(defsubst my-lint-layout-looking-at-statement-p ()
  "If `looking-at' at semicolon at the end of line."
  (my-lint-layout-save-point
    (goto-char (line-end-position))
    (search-backward ";" (line-beginning-position) t)))

(defsubst my-lint-layout-looking-at-variable-at-line-p ()
  "If `looking-at' at variable at line"
  (my-lint-layout-save-point
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

(defsubst my-lint-layout-result-header-string-insert ()
  "Insert file and time string."
  (insert (my-lint-layout-php-result-buffer-header)))

(defsubst my-lint-layout-result-sort-lines ()
  "Sort lines."
  (my-lint-layout-with-result-buffer
    (sort-lines (not 'reverse) (point-min) (point-max))))

(defun my-lint-layout-message (msg line &optional prefix)
  "Write MSG with LINE numnber using PREFIX.
See `my-lint-layout-buffer-name'."
  (my-lint-layout-with-result-buffer
    (goto-char (point-max))
    (insert (format "%s%04d: %s\n"
		    (my-lint-layout-prefix prefix)
		    line
		    msg))))

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

^ ^^
The leading indent is in submatch 1 and text start indent in 2."
  (if (looking-at "^\\([ \t]*\\)[*]\\([ \t]*\\)")
      (match-end 1)))

(defun my-lint-layout-run-list (list &optional prefix point)
  "Run LIST of functions from current point forward.
Point is preserved after each function. Result buffer is not
displayed."
  (dolist (function list)
    (my-lint-layout-save-point
      (if point
	  (goto-char point))
      (my-lint-layout-debug-message
       "debug layout: check %s %s" function prefix)
      (funcall function prefix))))

;;; ........................................................... &occur ...

(defconst my-lint-layout-php-check-regexp-occur-modern-style-list
  (list
   '("[a-z].*{[ \t\r\n]*$"
     "Possibly K&R brace style, expected line-up")

   '("[a-z].*}[ \t\r\n]*$"
     "Possibly K&R brace style, expected line-up")

   '("^[ \t]*function[ \t]+[a-z0-9_]+("
     "In funcdef, no space before starting paren")

   '("^[ \t]*[$][a-z0-9]+_[a-z0-9]+[ \t\r\n]*="
     "Variable name not CamelCase"
     nil
     (lambda ()
       (let* ((str   (match-string 0)))
	 (my-lint-layout-with-case
	   ;; Global variable
	   (not (string-match "[$][A-Z][A-Z][A-Z]" str))))))

   '("^[ \t]*function[ \t][a-z0-9]+_[^ \t]*[ \t]*("
     "In funcdef, name not CamelCase"))
  "Check Modern layout style.")

;; NOTES:
;; *) It's okay to use "@" suppression
;;
;;  $this->conn = @mysql_connect(DBHOST, DBUSER, DBPASS);
;;  if ( ! $this->conn )

(defconst my-lint-layout-php-check-regexp-occur-list
  (list

   ;; See PHP manual Security => Using Register Globals
   (list
    (concat
     "\\<[$]\\(HTTP\\)_[A-Z]+_[A-Z][A-Z_]+\\>")
    "Security risk. Superglobal variables may be supported in newest PHP")

   '("\\<register_globals[ \t\r\n]+="
     "Security risk. Function register_globals() is deprecated.")

   (list
    (concat
     "^[ \]*"
     "\\(class\\|function\\|if\\|elsewhile\\|abstract\\|interface\\|foreach\\)[ \t]*("
     "\\>")
    "Possibly misspelled keyword, expect lowercase"
    nil
    '(lambda ()
       (let* ((str   (match-string 0))
	      (lower (downcase str)))
	 (not (string= str lower)))))

   '("[|][|]\\|&&"
     "Consider readable alternative for relational op")

   '("^[ \t]*#"
     "Not a recommended comment style")

   '("^[ \t]*var[ \t]*[a-z]"
     "Old vardef. Migrate to syntax public|protected: ")

   ;; "$var" . "string"
   '("\"[$]_*[a-zA-Z0-9]+\""
     "Double quotes around simple variable not needed")

   '("\\<ereg[_a-z]*(.*)"
     "preg*() function family recommended for")

   '("\\<include[_a-z][( \t]*[\"\'$]"
     "require*() function family is safer than")

   '("\\<echo[( \t]*[\"\'$]"
     "Standard print() recommended for")

   '("\\<isset[( \t]*[\"\'$]"
     "Possible better alternative is to use empty() test")

   '("[.=][ \t]*\\<date(.*)"
     "POSIX standard strftime() recommended for")

   '("^[ \t]*else[ \t]+if\\>"
     "Standard elseif keyword not used")

   '("\\<\\(if\\|else\\|elseif\\)[ \t]*(.*[$a-z][ \t]*=[ \t]*[$a-z]"
     "Assignment inside statement"
     "mysql")

   '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)("
     "In statement, no space between keyword like 'if' and starting paren")

   '("\\<\\(if\\|else\\|else[ \t]*if\\|for\\|foreach\\|while\\)[ \t]*([^ \t\r\n]"
     "In statement, no space after starting paren")

   '("\\<\\(if\\|else\\|foreach\\|for\\|while\\)[ \t]*([^ $)\t\r\n]"
     "In statement, no space after keyword and paren")

   '("\\<\\(if\\|foreach\\|while\\)[ \t]*(.*[^ \t])[ \t]*$"
     "In statement, no space before closing paren")

   '("this->[^][ )\t\r\n]+[ \t]+("
     "In funcall, possibly extra space before opening paren")

   ;; this ->var;
   '("\\<$?this[ \t]+->[ \t]*[_a-zA-Z]"
     "In ref, leading space before token ->")

   ;; this ->var;
   '("\\<$?this->[ \t]+[_a-zA-Z]"
     "In ref, leading space after token ->")

   ;; funcall(code )
   '("\\<[_a-zA-Z][_a-zA-Z0-9>-]+([^)\r\n]*[ \t]+)"
     "In funcall, possibly extra space before closing paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; funcall( code)
   '("\\<[_a-zA-Z][_a-zA-Z0-9>-]+([ \t]+[^)\r\n]*)"
     "In funcall, possibly extra space after opening paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; code );
   '("[^) \t\r\n]+[ \t]+);"
     "In funcall, possibly extra space before closing paren (statement)"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; function ( param, def)
   (list
    (concat
     "^[ \t\r\n]*"
     my-lint-layout-generic-access-modifier-regexp "?[ \t\r\n]*"
     "function[ \t]+"
     ".*([ \t]")
     "In funcdef, extra space after opening paren")

   ;; function (param, def )
   (list
    (concat
     "^[ \t\r\n]*"
     my-lint-layout-generic-access-modifier-regexp "?[ \t\r\n]*"
     "function[ \t]+"
     ".*([^)\r\n]*[ \t])")
    "In funcdef, extra space before closing paren")

   (list
    (concat
     "[$][a-z0-9_>-]+[.][$\"][^);]"
     "\\|\"[.][$][a-z_]"
     "\\|[)][.][\"][^);]")
    "No surrounding spaces around concat(.)")

   ;; if ( $query and mysql_result == false )
   (list
    (concat
     "\\<\\(?:else[ \t]*if\\|if\\|foreach\\|while\\)[ \t]*("
     ".*[ \t][$][^ 0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|and\\|or\\)[ \t]+"
     "[a-z0-9_]+[) \t\r\n]")
    "Possibly missing vardef($) in relational test at right")

   ;; if ( value and $var )
   (list
    (concat
     "\\<\\(?:elseif\\|if\\|foreach\\|while\\)[ \t]*("
     "[ \t][^ $0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|\\<and\\>\\|\\<or\\>\\)[ \t]*"
     "[$][a-z0-9_]+[) \t\r\n]")
    "Possibly missing vardef($) in relational test at left")

   '("[$][a-z][_a-z0-9]*=[ \t]+[$a-z_0-9\"\'<]"
     "No space at left of equal sign")

   '("[$][a-z][_a-z0-9]*[ \t]+=[$a-z_0-9\"'<]"
     "No space at right of equal sign")

   '("[$][a-z][_a-z0-9]*=[$a-z_0-9\"'<]"
     "No spaces around equal sign")

   '("[!=]=[ \t]*\\(null\\|true\\|false\\)"
     "Possibly unnecessary test against literal"
     nil
     (lambda ()
       (not
	(or
	 (string-match "assert"
		       (my-lint-layout-current-line-string))
	 (save-excursion
	   (forward-line -1)
	   (string-match "assert"
			 (my-lint-layout-current-line-string)))))))

   '("\\<function[ \t]+\\(de\\|con\\)struct"
     "Possibly mispelled __(de|con)struct"))
  "Search ((REGEXP MESSAGE [NOT-REGEXP] [FUNC]) ..).")

(defun my-lint-layout-php-check-regexp-occur (&optional prefix list)
  "Check regepx in LIST or `my-lint-layout-php-check-regexp-occur-list'."
  (let (line)
    (dolist (elt (or list
		     my-lint-layout-php-check-regexp-occur-list))
      (multiple-value-bind (re msg not-re func) elt
	(save-excursion
	  (while (re-search-forward re nil t)
	    (setq line (my-lint-layout-current-line-string))
	    (when (and (not (my-lint-layout-string-comment-p line))
		       (or (null not-re)
			   (save-match-data
			     (not (string-match not-re line))))
		       (or (null func)
			   (funcall func)))
	      (my-lint-layout-message
	       (format "[code] %s: %s"
		       msg
		       (my-lint-layout-current-line-string))
	       (my-lint-layout-current-line-number)
	       prefix))))))))

(defun my-lint-layout-php-check-regexp-occur-main (&optional prefix)
  "Run all occur checks."
  (my-lint-layout-flet-run-at-point
    (run 'my-lint-layout-php-check-regexp-occur prefix)
    (run 'my-lint-layout-php-check-regexp-occur
	 prefix
	 my-lint-layout-php-check-regexp-occur-modern-style-list)))

(defun my-lint-layout-php-check-regexp-occur-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-php-check-regexp-occur'."
  (my-lint-layout-point-min
    (my-lint-layout-php-check-regexp-occur-main prefix)))

(defun my-lint-layout-php-check-regexp-occur-buffer-interactive ()
  "Call `my-lint-layout-php-check-regexp-occur-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-lint-layout-php-check-regexp-occur-buffer)))

;;; ............................................................ &misc ...

(defun my-lint-layout-generic-class-forward ()
  "Goto next class definition."
  (let ((class  (save-excursion (my-lint-layout-search-forward-class-beginning)))
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
       (my-lint-layout-current-line-number)
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
  (save-excursion
    (while (re-search-forward
	    ;;  3 x threshold
	    "^[ \t]*print(.*\n[ \t]*print(.*\n[ \t]*print(" nil t)
      (my-lint-layout-message
       "Multiple print*() calls. Alternative HERE syntax recommended."
       (my-lint-layout-current-line-number)
       prefix))))

(defsubst my-lint-layout-php-print-command-forward-1 ()
  "Find print or echo command."
  (re-search-forward
   ;; text/html; charset=utf-8
   "^[ \t]*\\(print\\|echo\\)[ \t]*[(\"][^;]+;" nil t))

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
      (unless (string-match "[$]\\|<<<" str) ;No variables used
	(setq lines (my-lint-layout-count-lines-in-string str))
	(when (> lines 3)
	  (my-lint-layout-message
	   "Possible maintenance problem, HERE doc syntax suggested (<<<)"
	   (- (my-lint-layout-current-line-number) lines)
	   prefix))))))

;;; ............................................................. &sql ...

(defun my-sql-re-search-forward ()
  "Search start of SQl keyword."
  )

(defun my-sql-re-search-backward ()
  "Search start of SQl keyword."
  )

(defun my-sql-layout-type-p ()
  "Determine type of SQL command."
  )

(defun my-sql-layout-interactive ()
  ""
  )

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
	 "SQL maintenance problem, better use HERE doc syntax (<<<)"
	 (- (my-lint-layout-current-line-number) lines)
	 prefix)))))

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
	    "Unknown opening short xml tag, expecting long <?tag (found: %s)"
	    (my-lint-layout-current-line-string))
	   (my-lint-layout-current-line-number)
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
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-php-check-control-statements (&optional prefix)
  "Keywords and preceding newlines."
  (let ()
    (while (re-search-forward
	    (concat
	     "\n[ \t]*\\([^ \t\r\n{]\\).*\n[ \t]*"
	     my-lint-layout-generic-control-statement-start-regexp
	     "[ \t]*(.*)")
	    nil t)
      ;; Ignore comments, xml-tags.
      (unless (save-excursion
		(goto-char (match-beginning 1))
		(my-lint-layout-current-line-string)
		(looking-at "[*/#]\\|[<>][?]"))
	(my-lint-layout-message
	 (concat
	  "[newline] no empty line found between "
	  "control statement and code above")
	 (my-lint-layout-current-line-number)
	 prefix)))))

(defun my-lint-layout-php-check-block-end-and-code (&optional prefix)
  "Block end followed by code immediately after.

	}
	return;
"
  (let (line
	str)
    (while (re-search-forward my-lint-layout-php-brace-and-code-regexp nil t)
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
	 line
       prefix)))))

(defun my-lint-layout-php-check-comment-statements (&optional prefix)
  "Check comment markers."
  (let ((re "^[ \t]*\\([#]\\|//\\|/[*][^*]\\)")
	str)
    (while (re-search-forward re nil t)
      (setq str (match-string 1))
      (when (string-match "#" str)
	(my-lint-layout-message
	 "[comment] unknown syntax."
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (looking-at "[^ \t\r\n]")
	(my-lint-layout-message
	 "[comment] no space between comment marker and text"
	 (my-lint-layout-current-line-number)
	 prefix))
      ;; Peek previous line
      (save-excursion
	(forward-line -1)
	(unless (looking-at (concat re "\\|^[ {}\t\r]*$"))
	  (my-lint-layout-message
	   "[comment] no empty line before comment start"
	   (1+ (my-lint-layout-current-line-number))
	   prefix))))))

(defun my-lint-layout-php-doc-above-p ()
  "Check if phpdoc is in above line."
  (let ((type (my-lint-layout-php-doc-examine-typeof
	       (my-lint-layout-current-line-string))))
    (save-excursion
      (goto-char (line-beginning-position))
      (skip-chars-backward "  \t\r\n") ;;  At the end of "*/"
      (goto-char (line-beginning-position))
      (looking-at "^[ \t]*[*]/[ \t]*$"))))

(defsubst my-lint-layout-php-re-search-forward-doc-keyword ()
  "Search `my-lint-layout-php-doc-location-regexp'."
  (re-search-forward
   my-lint-layout-php-doc-location-regexp
   nil t))

(defun my-lint-layout-php-class-check-variables (&optional prefix)
  "Check class variables."
  (let (class-p
	max
	point
	str
	line)
    (save-excursion
      (setq point (point))
      (setq class-p (my-lint-layout-search-forward-class-p))
      (goto-char point)
      ;;  one class - One file assumption. If there are no methods, this is
      ;;  pure variable class, like struct.
      (setq max (or (my-lint-layout-php-search-forward-function-beginning)
		    (point-max)))
      (when class-p
	(goto-char point)
	(while (my-lint-layout-search-forward-variable-dollar-beginning max)
	  (setq str  (my-lint-layout-current-line-string)
		line (my-lint-layout-current-line-number))
	  (cond
	   ((string-match "\\<var\\>" str)
	    (my-lint-layout-message
	     (concat
	      "[code] Deprecated 'var'; expecting "
	      "private, public etc. access modifiers")
	     line prefix))
	   ((string-match
	     my-lint-layout-generic-access-modifier-regexp
	     str)) ;; OK, do nothing
	   (t
	    (my-lint-layout-message
	     (concat
	     "[code] Possibly missing access modifier "
	     "like private, public etc.")
	     line prefix))))))))

(defun my-lint-layout-php-check-doc-missing (&optional prefix)
  "Check missing documentation."
  (let (class-p
	function-p
	str
	line)
    (save-excursion
      (setq class-p (my-lint-layout-search-forward-class-p)))
    (while (my-lint-layout-php-re-search-forward-doc-keyword)
      (setq str  (my-lint-layout-current-line-string)
	    line (my-lint-layout-current-line-number))
      (unless (my-lint-layout-php-doc-above-p)
	(cond
	 ;; if (...)
	 ;;    require "this" . ar;
	 ((and (my-lint-layout-conditional-above-p)
	       (my-lint-layout-looking-at-variable-at-line-p)))
	 ((and class-p
	       (my-lint-layout-type-include-string-p str))
	  (my-lint-layout-message
	   "[phpdoc] require or include not documented"
	   line prefix))
	 ((my-lint-layout-type-function-string-p str)
	  (my-lint-layout-message
	   "[phpdoc] function not documented"
	   line prefix))
	 ;; Skip "function files" FIXME: ???
	 ((my-lint-layout-type-include-string-p str)
	  (my-lint-layout-message
	   "[phpdoc] require or include not documented (non-class context)"
	   line prefix))
	 ((my-lint-layout-type-statement-string-p str)
	  (my-lint-layout-message
	   "[phpdoc] variable not documented"
	   line prefix)))))))

(defsubst my-lint-layout-php-indent-level (str)
  "Count indent."
  (and str
       (setq str (replace-regexp-in-string "\t" "        " str))
       (string-match "^[ \t]*" str)
       (length (match-string 0 str))))

(defsubst my-lint-layout-php-statement-brace-forward (&optional brace)
  "Find statement block BRACE, which is '{' by default."
  ;;  Notice that BRACE is hre used in regexp.
  ;;
  ;; if ( preg_match("^[0-9]{1,9}$", $bfield ) )
  ;; {
  (let ((skip-chars "^{")
	(re "{[ \t]*$"))
    (when brace
      ;; FIXME: Not working
      (setq skip-chars "^}")
      (setq re "}[ \t]*\\(.*//.*\\)$"))
    (forward-char 1)
    (while (and (not (eobp))
		(if (looking-at re)
		    nil
		  (progn
		    (forward-char 1)
		    t)))
      (skip-chars-forward skip-chars))))

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
  (let ((i      (my-lint-layout-php-indent-level str))
	(istep  my-lint-layout-generic-indent-step))
    (when (numberp i)
      (cond
       ((and (or (null base-indent)
		 (< i (+ base-indent istep)))
	     (not (zerop (mod i 4))))
	(my-lint-layout-message
	 (format (concat "[code] Possibly incorrect indent "
			 "at col %d where multiple of %d expected")
		 i
		 istep)
	 (my-lint-layout-current-line-number)
	 prefix))
       ((and base-indent
	     (not (zerop i))
	     (eq i base-indent))
	(my-lint-layout-message
	 (format "[code] Possibly missing indentation at col %d" i)
	 (my-lint-layout-current-line-number)
	 prefix))))))

(defun my-lint-layout-php-check-keywords-case (keyword fullstr &optional prefix)
  "If statement: check proper and, or, true, false character case."
  (my-lint-layout-with-case
    (if (string-match
	 "<\\(AND\\|OR\\|FALSE\\|TRUE\\|NULL\\)\\>"
	 fullstr)
	(my-lint-layout-message
	 (format "[code] Keyword in conditional; expecting lowercase '%s'"
		 (match-string 0 fullstr))
	 ;;  Point is at brace, refer to above line.
	 (1- (my-lint-layout-current-line-number))
	 prefix))))

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

(defun my-lint-layout-php-statement-brace-and-indent (&optional prefix)
  "Check that code is indented according to brace column at point."
  (save-excursion
    (let ((beg (line-end-position)))
      (when (setq brace-end-point
		  (my-lint-layout-php-statement-brace-end-forward))
	(my-lint-layout-php-statement-brace-block-check
	 beg
	 brace-end-point
	 (current-column)
	 prefix)))))

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
     (concat
      "Misplaced comment. Should be inside "
      (if str
	  (format "'%s' block" str)
	"next brace block"))
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-lint-layout-php-check-statement-continue-detach (str &optional prefix)
  "At statement STR, like 'else', peek above line."
  (forward-line -1)
  (unless (looking-at "^[ \t]*}")
    (my-lint-layout-message
     (format "keyword '%s' is not attached to brace block"
	     str)
     (my-lint-layout-current-line-number)
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
     (my-lint-layout-current-line-number)
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

(defun my-lint-layout-php-check-statement-start-2 (&optional prefix)
  "Check incorrect statement like:

if ( check );
{
    line
}"
  (let (str)
    (while (my-lint-layout-php-statement-forward)
      (when (looking-at ";")
	(my-lint-layout-message
	 (format "Possibly misplaced semicolon: %s"
		 (my-lint-layout-current-line-string))
	 (my-lint-layout-current-line-number)
	 prefix)))))

(defsubst my-lint-layout-php-brace-statement-forward (&optional max)
  "Search control statement with brace forward."
  (re-search-forward
   my-lint-layout-generic-statement-regexp-brace
   max
   t))

(defun my-lint-layout-php-check-statement-end (&optional prefix)
  "Check end of line for ';' and whitespace."
  (let (str
	line)
    (while (my-lint-layout-search-forward-variable-dollar-beginning)
      (setq str (my-lint-layout-current-line-string))
      (when (string-match "[ \t];[ \t]*$" str)
	;;  "$a = 12 ;"  vs. "$a = 12;"
	(setq line (my-lint-layout-current-line-number))
	(my-lint-layout-message
	 "[code] Extra whitespace before statement end(;)"
	 line prefix)))))

(defun my-lint-layout-php-check-statement-start (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
	 keyword
	 fullstr
	 point
	 kwd-point
	 indent
	 comment-p
	 continue-p
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
	    indent    (my-lint-layout-save-point
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
	(setq statement-start-col (current-column)
	      statement-line      (my-lint-layout-current-line-number))
	(when continue-p
	  (goto-char point)
	  (my-lint-layout-php-check-statement-continue-detach keyword prefix)
	  (goto-char point)
	  (my-lint-layout-php-check-statement-comment-above keyword prefix)))
      (my-lint-layout-php-check-indent-string-check indent statement-line prefix)
      ;; (my-lint-layout-php-statement-brace-forward)
      ;; brace-start-line (my-lint-layout-current-line-number))
      (my-lint-layout-php-statement-brace-and-indent prefix)
      (my-lint-layout-php-check-statement-brace-detach fullstr)
      (when (string-match "\\<if\\>\\|\\<els.*if\\>" keyword)
	(my-lint-layout-php-check-keywords-case keyword fullstr prefix))
      (unless (eq statement-start-col brace-start-col)
	(my-lint-layout-message
	 (format "[code] brace { not directly under keyword '%s', expect col %d"
		 (or keyword "")
		 statement-start-col)
	 (my-lint-layout-current-line-number)
	 prefix)))))

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
	    nil
	    t)
      (my-lint-layout-message
       (format "[misc] Possible unresolved conflict: %s"
	       (my-lint-layout-current-line-string))
       (my-lint-layout-current-line-number)
       prefix))))

;;; ........................................................ &keywords ...

(defun my-lint-layout-php-check-keyword-spelling-lowercase
  (str &optional prefix)
  "Check lowercase."
  (unless (string= (downcase str)
		   str)
    (my-lint-layout-message
     (format "[misc] Lowercase keyword expected for `%s'"
	     str)
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-lint-layout-php-check-keywords-error-opening-paren-leading
  (str &optional prefix)
  "Error: <keyword><space>(); Leading <space>."
  (my-lint-layout-message
   (format "[misc] In funcall, `%s' and extra space before opening paren"
	   str)
   (my-lint-layout-current-line-number)
   prefix))

(defun my-lint-layout-php-check-keywords-error-opening-paren-trailing
  (str &optional prefix)
  "Error: <keyword>(<space>...; trailing <space>."
  (my-lint-layout-message
   (format "[misc] In funcall, `%s' and extra space after opening paren"
	   str)
   (my-lint-layout-current-line-number)
   prefix))

(defun my-lint-layout-php-check-keywords-error-closing-paren-leading
  (str &optional prefix)
  "Error: <keyword>(...<space>); trailing <space>."
  (my-lint-layout-message
   (format "[misc] In funcall, `%s' and extra space before closing paren"
	   str)
   (my-lint-layout-current-line-number)
   prefix))

(defun my-lint-layout-php-check-keywords-main (&optional prefix keyword-re)
  "Check correct lowercase spelling.
KEYWORD-RE defaults to `my-lint-layout-php-function-call-keywords-list'."
  (let (class-p
	function-p
	str
	re
	indent
	line)
    (save-excursion
      (setq class-p (my-lint-layout-search-forward-class-p)))
    (or keyword-re
	(setq keyword-re my-lint-layout-php-function-call-keywords-generic))
    (save-excursion
      (while (re-search-forward keyword-re nil t)
	(setq str (match-string 0))
	(when (looking-at "\\([ \t]*\\)(")
	  (setq indent (match-string 1))
	  (my-lint-layout-php-check-keyword-spelling-lowercase str prefix)
	  (when (> (length indent) 0)
	    (my-lint-layout-php-check-keywords-error-opening-paren-leading
	     str prefix))
	  (when (looking-at "[ \t]*([ \t]")
	    (my-lint-layout-php-check-keywords-error-opening-paren-trailing
	     str prefix))
	  (when (looking-at "[ \t]*([^)\r\n]+[ \t])")
	    (my-lint-layout-php-check-keywords-error-closing-paren-leading
	     str prefix)))))))

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
	   (format "[misc] Possibly unfilled template: %s" str)
	 (format "[misc] Mispelled word: %s" str))
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-php-check-words-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-php-check-words'."
  (my-lint-layout-point-min
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
	 (format "[misc] No exact EOF marker found: '%s'"
		 my-lint-layout-eof-regexp)
	 (my-lint-layout-current-line-number)
	 prefix)))))

(defun my-lint-layout-check-eof-marker-interactive ()
  "Near the last line of file find text `my-lint-layout-eof-regexp'."
  (interactive)
  (my-lint-layout-check-eof-marker))

;;; ...................................................... &whitespace ...

(defun my-lint-layout-whitespace-extra-newlines (&optional msg prefix)
  "Check extra newlines at point."
  (when (looking-at "\\(\\(?:[ \t]*\r?\n\\)+\\)")
    (let ((str (match-string 0))
	  (line (my-lint-layout-current-line-number)))
      (my-lint-layout-message
       (format "[newline] extra newline %d%s"
	       (my-lint-layout-count-lines-in-string str)
	       (or msg ""))
       line prefix))))

(defun my-lint-layout-whitespace-trailing (&optional prefix)
  "Check trailing whitespace."
  (let (line)
    (while (re-search-forward "[ \t]+$" nil t)
      (setq line (my-lint-layout-current-line-number))
      (my-lint-layout-message
       "[whitespace] trailing whitepace at end of line"
       line prefix))))

(defun my-lint-layout-whitespace-multiple-newlines (&optional prefix)
  "Check multiple newlines."
  (let (line)
    (while (re-search-forward "^[ \t]*\r?\n\\([ \t]*\r?\n\\)+" nil t)
      (my-lint-layout-message
       (format "[newline] extra newline(s) %d found above"
	       (1- (my-lint-layout-count-lines-in-string
		    (match-string 0))))
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-whitespace-at-eob (&optional prefix)
  "Check multiple newlines."
  (let (line)
    (goto-char (point-max))
    (when (re-search-backward "[^ \t\r\n]\\(\n\\)?" nil t)
      (cond
       ((string= "\n" (match-string 1))
	;; eob trailing newlines?
	(forward-line 1)
	(my-lint-layout-whitespace-extra-newlines
	 " at end of file" prefix))
       (t
	(setq line (my-lint-layout-current-line-number))
	(my-lint-layout-message
	 "[newline] missing newline from last line of file"
	 line prefix))))))

(defun my-lint-layout-check-whitespace (&optional prefix)
  "Check whitespace problems: eol, bob, eob from current point."
  (save-excursion
    (save-excursion
      (my-lint-layout-whitespace-multiple-newlines prefix))
    (my-lint-layout-whitespace-trailing prefix)
    (my-lint-layout-whitespace-at-eob prefix)))

(defun my-lint-layout-check-whitespace-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-check-whitespace'."
  (my-lint-layout-point-min
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
  (my-lint-layout-point-min
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
	 (my-lint-layout-current-line-number)
	 prefix)))))

;;; .......................................................... &length ...

(defun my-lint-layout-check-line-length (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
	 (re (concat "^"
		     (make-string col ?\.)
		     "\\(.+\\)")))
    (while (re-search-forward re nil t)
      (my-lint-layout-message
       (format "line lenght past column %d: %s" col (match-string 1))
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-check-line-length-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-check-line-length'."
  (my-lint-layout-point-min
    (my-lint-layout-check-line-length)))

(defun my-lint-layout-check-line-length-buffer-interactive (&optional prefix)
  "Run `my-lint-layout-check-line-length-buffer' and show results."
  (interactive)
  (my-lint-layout-check-line-length-buffer))

;;; ..................................................... &gpl-license ...

(defsubst my-layout-license-gpl-search-forward ()
  "Position point to License line."
  (re-search-forward "\\<GNU General Public License\\>" nil t))

(defun my-layout-license-not-exists (&optional prefix)
  "Check if License exists."
  (unless (my-layout-license-gpl-search-forward)
    (my-lint-layout-message
     "[licence] GNU General Public License not found."
     1
     prefix)
    t))

(defun my-layout-license-text (text &optional prefix)
  "Check that License TEXT exists."
  (unless (re-search-forward (regexp-quote text) nil t)
    (my-lint-layout-message
     (format "[licence] text not found: %s..." text)
     1
     prefix)))

(defun my-layout-license-check-main (&optional prefix)
  "Check License syntax.
Optional PREFIX is used add filename to the beginning of line."
  (when (my-lint-layout-buffer-data-p)
    (if (my-layout-license-not-exists prefix)
	t
      ;; The order is important
      (my-layout-license-text
       "published by the Free Software Foundation" prefix)
      (my-lint-layout-with-case
	(my-layout-license-text
	 "WITHOUT ANY WARRANTY" prefix))
      (my-layout-license-text
       "You should have received a copy" prefix)
      (my-layout-license-text
       "http://www.gnu.org/licenses" prefix))))

(defun my-layout-license-check-buffer (&optional prefix)
  "Check from `point-min' with `my-layout-license-check-main'."
  (my-lint-layout-point-min
    (my-layout-license-check-main prefix)))

(defun my-layout-license-check-main-interactive (&optional prefix)
  "Call `my-layout-license-check-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-layout-license-check-buffer)))

;;; ....................................................... &copyright ...

(defsubst my-layout-copyright-line-p ()
  "Check if current point is copyright line.
Should be called right after `my-layout-copyright-search-forward'."
  (not (looking-at ".*information")))

(defsubst my-layout-copyright-search-forward ()
  "Position point to 'Copyright' line."
  (let (moved)
    (while (and (setq moved
		      (re-search-forward "\\<copyright\\>" nil t))
		(not (my-layout-copyright-line-p))))
    (and moved
	 (my-layout-copyright-line-p))))

(defun my-layout-copyright-not-exists (&optional prefix)
  "Check if Copyright exists."
  (unless (my-layout-copyright-search-forward)
    (my-lint-layout-message
     "[copyright] Not found"
     1
     prefix)
    t))

(defun my-layout-copyright-line-syntax (&optional prefix)
  "Check Copyright line syntax."
  (let ((line (my-lint-layout-current-line-number))
	(string (my-lint-layout-current-line-string)))
    (when (and (not (looking-at " +\\((C)\\|&copy;\\)"))
	       (not (string-match "@copyright" string)))
      (my-lint-layout-message
       (format "[copyright] expecting (C) sign: %s" string)
       line
       prefix))
    (unless (looking-at ".*[0-9][0-9][0-9][0-9]")
      (my-lint-layout-message
       (format "[copyright] missing year: %s" string)
       line
       prefix))
    (when (and (not (looking-at ".*<.+>"))
	       ;;  Tag "@copyright ....."
	       (not (string-match "@copyright" string)))
      (my-lint-layout-message
       (format "[copyright] missing email address: %s" string)
       line
       prefix))
    (when (and (looking-at ".*<\\(.+\\)>")
	       (string-match
		"foo\\|bar\\|quux\\|example"
		(match-string 1)))
      (my-lint-layout-message
       (format "[copyright] email is template: %s" string)
       line
       prefix))
    (when (looking-at ".*,")
      (my-lint-layout-message
       (format
	"[copyright] only one person should be listed in Copyright line: %s"
	string)
       line
       prefix))))

(defun my-layout-copyright-check-main (&optional prefix)
  "Check Copyright syntax.
Optional PREFIX is used add filename to the beginning of line."
  (if (my-layout-copyright-not-exists prefix)
      t
    (my-layout-copyright-line-syntax prefix)
    (while (my-layout-copyright-search-forward)
      (my-layout-copyright-line-syntax prefix))))

(defun my-layout-copyright-check-buffer (&optional prefix)
  "Check from `point-min' with `my-layout-copyright-check-main'."
  (my-lint-layout-point-min
    (my-layout-copyright-check-main prefix)))

(defun my-layout-copyright-check-main-interactive ()
  "Call `my-layout-copyright-check-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-layout-copyright-check-buffer)))

;;; ....................................................... &changelog ...

(defconst my-layout-changelog-item-regexp
  "^[ \t][*]\\( *\\)\\([^ :()\t\r\n]+\\)")

(defconst my-layout-changelog-wordlist-regexp
  "\\(\
\\<[a-z]+ed\\>\
\\|<[a-z]+ing\\>\
\\)")

(defun my-layout-word-tense (word message) ;Primitive
  "Check non-active tense."
  (when (and word
	     (string-match "\\(ed\\|ing\\)$" word))
    (my-lint-layout-message
     message
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-layout-changelog-wording (&optional prefix)
  "Search for words in non-active tense."
  (while (re-search-forward my-layout-changelog-wordlist-regexp nil t)
    (my-lint-layout-message
     (format "[changelog] word possibly in wrong tense '%s'"
	     (match-string 0))
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-layout-changelog-file-items (&optional prefix)
  "Check ChangeLog syntax. The wording: Add, Modify, Change ..."
  (let (change
	word)
    (while (re-search-forward "(\\([^)\r\n]+\\)):" nil t)
      (setq change (match-string 1)
	    word   (and (looking-at " *\\([^ \t\r\n]+\\)")
			(match-string 1)))
      (when word
	(my-layout-word-tense
	 word
	 (format "[changelog] change marker, wrong tense of verb '%s'" word)))
      (when (looking-at "  ")
	(my-lint-layout-message
	 (format
	  "[changelog] change marker, extra spaces after '(%s):'"
	  change)
	 (my-lint-layout-current-line-number)
	 prefix))
      (unless (looking-at "[ \r\n]")
	(my-lint-layout-message
	 (format
	  "[changelog] change marker, need one space after '(%s):'"
	  change)
	 (my-lint-layout-current-line-number))))))

(defun my-layout-changelog-file-bullet (&optional prefix)
  "Check ChangeLog syntax. The filename line:

  * application/template/overview.php: Add new file.

Optional PREFIX is used add filename to the beginning of line."
  (let (indent
	word
	file)
    (while (re-search-forward my-layout-changelog-item-regexp nil t)
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
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (looking-at ":?[ \t]*([^)\r\n]+):  ")
	(my-lint-layout-message
	 "[changelog] at *, extra space after (marker):"
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (looking-at ":?[ \t]*([^)\r\n]+):[^ \t\r\n]")
	(my-lint-layout-message
	 "[changelog] at *, no space after (marker):"
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (and (not (looking-at ":?[ \r\n]")))
	(my-lint-layout-message
	 "[changelog] at *, need one space after pathname"
	 (my-lint-layout-current-line-number)
	 prefix))
      (when word
	(my-layout-word-tense
	 word
	 (format "[changelog] at *, wrong tense of verb '%s'" word)))
      (when (and (string-match " " indent)
		 (not (string= " " indent)))
	(my-lint-layout-message
	 "[changelog] not exactly one space after character '*'"
	 (my-lint-layout-current-line-number)
	 prefix))
      (forward-line 1))))

(defun my-layout-changelog-check-main (&optional prefix)
  "Check ChangeLog syntax.
Optional PREFIX is used add filename to the beginning of line."
  (my-lint-layout-generic-check-mixed-eol-crlf prefix)
  (my-layout-changelog-file-bullet prefix)
  (my-layout-changelog-file-items prefix)
  (my-layout-changelog-wording prefix))

(defun my-layout-changelog-check-standard-main (&optional prefix)
  "Check ChangeLog syntax. With standard checks."
  (my-layout-changelog-check-main prefix)
  (my-lint-layout-check-whitespace-buffer prefix)
  (my-lint-layout-check-line-length prefix))

(defun my-layout-changelog-check-main-interactive ()
  "Call my-layout-changelog-check-main and other relevant checks."
  (interactive)
  (my-lint-layout-with-interactive
    (my-layout-changelog-check-standard-main)))

;;; ........................................................... &brace ...

(defsubst my-lint-layout-php-brace-forward-1 ()
  "Move to start brace {"
  ;; {
  ;;
  ;;    if ()
  (and (re-search-forward "[{][ \t]*\n[ \t]*\\(\r?\n[ \t\r\n]+\\)[^{}]" nil t)
       (list (point) 'beg)))

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
	(concat "}" my-lint-layout-php-brace-and-code-regexp)
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
	(my-lint-layout-message format line prefix)))))

(defun my-lint-layout-php-check-brace-extra-newline (&optional prefix)
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
  (eval-when-compile
    (concat
     "\\b"
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
      t) "\\b"))
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-functions
  (eval-when-compile
    (concat
     "\\b"
     (regexp-opt
      '("AVG"
	"COUNT"
	"MAX"
	"MIN"
	"SUM"
	)
      t)
     "\\b"))
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-column-mysql
  (eval-when-compile
    (concat
     "\\b"
     (regexp-opt
      '("AUTO_INCREMENT"
	"UNSIGNED"
	"ZEROFILL"
	;; CREATE TABLE xxx (...) ENGINE = InnoDB;
	"ENGINE"
	"InnoDB"
	) t) "\\b"))
  "MySQL column keywords.")

(defconst my-lint-layout-sql-keywords-sql92-data-types
  (eval-when-compile
    (concat
     "\\b"
     (regexp-opt
      '(
	"BIT"
	"CHAR"
	"CHARACTER"
	"CURRENCY"
	"DATE"
	"DEC"
	"DECIMAL"
	"DOUBLE PRECISION"
	"FLOAT"
	"INT"
	"INTEGER"
	"INTERVAL"
	"MONEY"
	"NCHAR"
	"NATIONAL"
	"NUMERIC"
	"REAL"
	"SMALLINT"
	"TIME"
	"TIMESTAMP"
	"VARCHAR"
	"VARYING"
	)
      t) "\\b"))
  "SQL reserved keywords.")

;; BIT data type in MS SQL Server stores a bit of data (0 or 1) and
;; does not correspond to previously described SQL99 BIT. The literal
;; value for bit is a single character from its range optionally
;; enclosed into single quotes.
;;
;; SMALLINT is virtually same as INTEGER, but maximum precision can be
;; smaller than that for INTEGER.

(defconst my-lint-layout-sql-keywords-sql99-data-types
  (eval-when-compile
    (concat
     "\\b"
     (regexp-opt
      '("ARRAY"
	"BOOLEAN"
	"BLOB"
	"CLOB"
	"LIST"
	"SET"
	"SMALLINT"
	)
      t) "\\b"))
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-sql-types
  (concat
   "\\("
   my-lint-layout-sql-keywords-sql92-data-types
   "\\|"
   my-lint-layout-sql-keywords-sql99-data-types
   "\\)")
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-all
  (concat
   "\\("
   my-lint-layout-sql-keywords-reserved
   "\\|"
   my-lint-layout-sql-keywords-functions
   "\\|"
   my-lint-layout-sql-keywords-sql92-data-types
   "\\|"
   my-lint-layout-sql-keywords-sql99-data-types
   "\\)")
  "SQL reserved keywords.")

(defsubst my-lint-layout-sql-backquote (str)
  "Check unknown backquote character.
MySQL:
  CREATE TABLE  `service`
  (
    `id`         INT(10)      NOT NULL,
    PRIMARY KEY  (`id`)
  );"
  (when (string-match "[`]" str)
    (my-lint-layout-message
     "[sql] Non-standard backquote character"
     (my-lint-layout-current-line-number)
     prefix)))

(defsubst my-lint-layout-sql-comment (str)
  "Check non-standard comment syntax."
  (when (string-match "#\\|/[*]" str)
    (my-lint-layout-message
     (format "[sql] non-standard comment syntax: %s" str)
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-lint-layout-sql-check-keywords (&optional prefix)
  "Check SQL syntax."
  (require 'sql)
  (let* ((sql-re         my-lint-layout-sql-keywords-all)
	 (type-sql92-re  my-lint-layout-sql-keywords-sql92-data-types)
	 (type-re        my-lint-layout-sql-keywords-sql-types)
	 ;;  "VARCHAR (80)"
	 (type-re-space (concat type-re "[ \t]+([ \t]*[0-9]+[ \t]*)"))
	 (mysql-re my-lint-layout-sql-keywords-column-mysql)
	 (step my-lint-layout-generic-indent-step)
	 mysql-p
	 non-std-kwd-p
	 non-std92-kwd-p
	 str
	 tmp
	 datatype
	 word)
    (while (re-search-forward "^\\([ \t]*[^-\r\n].*\\)" nil t)
      (setq str (match-string 1))
      (setq non-std-kwd-p nil)
      (my-lint-layout-sql-backquote str)
      (my-lint-layout-sql-comment str)
      (when (string-match mysql-re str)
	(setq non-std-kwd-p t)
	(my-lint-layout-message
	 (format "[sql] Non-standard keyword: %s" (match-string 0 str))
	 (my-lint-layout-current-line-number)
	 prefix))
      (my-lint-layout-with-case
	(when (and (not non-std-kwd-p)
		   (string-match "[^ \t\r\n]*[a-z]+[A-Z]+[^ \t\r\n]*" str))
	  (my-lint-layout-message
	   (format "[sql] Portability problem in mixed case name: %s"
		   (match-string 0 str))
	   (my-lint-layout-current-line-number)
	   prefix)))
      (when (string-match type-re-space str)
	(my-lint-layout-message
	 (format "[sql] Extra space found near size definition: %s"
		 (match-string 0 str))
	 (my-lint-layout-current-line-number)
	 prefix))
      (setq word nil)
      (cond
       ((string-match "^[ \t]*\\(.+[^ \t]+\\)[ \t]+NOT[ \t]+NULL" str)
	(setq tmp (match-string 1 str))
	;; FIXME "FLOAT(1, 2)"
	(when (string-match
	       "\\(?:\\([a-z]+\\)\\(([ \t]*[,0-9 \t]+)\\)?\\)$"
	       tmp)
	  (setq word (match-string 1 tmp))))
       ((and (string-match "NULL" str)
	     (not (string-match "NOT[ \t]+NULL" str)))
	(when (string-match "NULL" str) ;; FIXME: NULL itself is not SQL92
	  (my-lint-layout-message
	   (format "[sql] Non-standard NULL keyword: %s"
		   str)
	   (my-lint-layout-current-line-number)
	   prefix))))
      (when word
	(when (and (null non-std-kwd-p) ; Not yet checked
		   (not (string-match type-re word)))
	  (setq non-std-kwd-p t)
	  (my-lint-layout-message
	   (format "[sql] Non-standard keyword or datatype: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	(when (and (null non-std-kwd-p)
		   (not (string-match type-sql92-re word)))
	  (my-lint-layout-message
	   (format "[sql] Non-SQL92 may cause portability problems: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	;; FIXME: tests only datatype now
	(when (and (string-match sql-re word)
		   (my-lint-layout-with-case
		     (not (string-match sql-re word))))
	  (my-lint-layout-message
	   (format "[sql] Keyword not in uppercase: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	(unless (string-match "^[ \t]+" str)
	  (my-lint-layout-message
	   (format "[sql] Statement not indented (by %d)" step)
	   (my-lint-layout-current-line-number)
	   prefix))))))

(defun my-lint-layout-sql-create-table (table)
  "Check CREATE TABLE."
  (my-lint-layout-with-case
    (when (string-match "[A-Z]" table)
      (my-lint-layout-message
       (format "[SQL] Portability problem, mixed case table name: %s" table)
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-sql-check-create-table (&optional prefix)
  "Check SQL syntax."
  (let (line
	table)
    (while (re-search-forward
	    "^[ \t]*\\(CREATE[ \t]+TABLE\\)[ \t]+\\([^ \t\r\n]+\\).*" nil t)
      (setq line  (match-string 0)
	    table (match-string 2))
      (my-lint-layout-sql-create-table table)
      (when (string-match "[`]" line)
	(my-lint-layout-message
	 "[sql] Non-standard backquote character"
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (string-match "[(]" line)
	(my-lint-layout-message
	 "[sql] Misplaced starting paren (possibly not lined-up)"
	 (my-lint-layout-current-line-number)
	 prefix)))))

(defun my-lint-layout-sql-check-batch-all (&optional prefix)
  "Check Css"
  (my-lint-layout-run-list
   my-lint-layout-check-sql-functions prefix))

(defun my-lint-layout-sql-buffer (&optional prefix)
  "Check from `point-min' with `my-lint-layout-sql-check-batch-all'."
  (my-lint-layout-point-min
    (my-lint-layout-sql-check-batch-all)))

(defun my-lint-layout-sql-buffer-interactive (&optional prefix)
  "Run `my-lint-layout-sql-buffer'."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-sql-buffer prefix)))

;;; ............................................................. &css ...

(defsubst my-lint-layout-php-check-multiple-statements-error (msg &optional prefix)
  "Write error."
  (my-lint-layout-message
   msg
   (my-lint-layout-current-line-number)
   prefix))

(defsubst my-lint-layout-php-check-multiple-statements (msg &optional prefix)
  "Check multiple ';'."
  (when (looking-at ".+;.*;")
    (my-lint-layout-php-check-multiple-statements-error msg prefix)))

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
       (format "[css] Body is not indented by %d at col %s" step col)
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-css-attribute (&optional prefix)
  "Check attribute."
  (when (and (looking-at "\\(.*[a-z]\\):")
	     ;;  a:hover
	     (not (string-match "\\<a" (match-string 1)))
	     (looking-at "\\([^ \t\r\n]+[a-z]:\\([^ \t\r\n]+\\)\\)"))
    (my-lint-layout-message
     (format "[css] No space between colon and attribute value: %s"
	     (match-string 1))
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-lint-layout-css-color (&optional prefix)
  "Check color-attribute."
  (let (str)
    (when (and (looking-at ".*\\<color:[ \t]*#\\([a-f0-9]+\\)")
	       (setq str (match-string 1))
	       (not (eq 6 (length str))))
      (my-lint-layout-message
       (format "[css] Color is not complete 6 digit hex value: %s"str)
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-css-body (&optional prefix)
  "Check body."
  (while (and (not (eobp))
	      (not (looking-at ".*[ \t][{}]")))
    (my-lint-layout-css-indent-level prefix)
    (my-lint-layout-css-attribute prefix)
    (my-lint-layout-css-color prefix)
    (my-lint-layout-php-check-multiple-statements
     "[css] Multiple attribute definitions (only one expected)"
     prefix)
    (forward-line 1)))

(defun my-lint-layout-css-skip-comment ()
  "Skip over comment if any."
  (when (looking-at "^[ \t]*/[*]")
    (search-forward "*/")))

(defun my-lint-layout-css-check-generic (&optional prefix)
  "Check Css"
  (let (str
	col
	len
	line
	statement-p)
    (while (re-search-forward "{\\([ \t\r\n]*\\)" nil t)
      (setq col      (current-column)
	    str      (match-string 1)
	    line     (my-lint-layout-current-line-number)
	    lines    (my-lint-layout-count-lines-in-string str))
      (my-lint-layout-css-skip-comment)
      (cond
       ((eq lines 0)
	(my-lint-layout-message
	 "[css] No newline after token '{'"
	 line prefix))
      ((> lines 1)
	(my-lint-layout-message
	 (format "[css] Extra %d empty lines after token '{'" (1- lines))
	 line prefix)))
      (my-lint-layout-current-line-string)
      (when (eq col 0)
	(my-lint-layout-message
	 (format "[css] not indented (by %d)"
		 my-lint-layout-generic-indent-step)
	 line prefix))
      ;;   background-color: #F8F8F8; border: 1px;
      (my-lint-layout-php-check-multiple-statements
       "[css] Multiple attribute definitions (only one expected)"
       prefix)
      (my-lint-layout-css-indent-level prefix)
      (my-lint-layout-css-body prefix))))

(defun my-lint-layout-check-comment-javadoc-invalid (&optional prefix)
  "Check invalid Javadoc-style /** and @tag in comments."
  (let ((re1 (concat "^[ \t]*" (regexp-quote "/**")))
	(re2 "^[ \t]*[*][ \t]*@[a-z]"))
    (dolist (re (list re1 re2))
      (save-excursion
	(while (re-search-forward re nil t)
	  (my-lint-layout-message
	   (format "[css] Probably misplaced PHP-style doc-block: %s"
		   (my-lint-layout-current-line-string))
	   (my-lint-layout-current-line-number)
	   prefix))))))

(defun my-lint-layout-css-check-batch-all (&optional prefix)
  "Check Css"
  (my-lint-layout-run-list
   my-lint-layout-check-css-functions prefix))

(defun my-lint-layout-css-check-buffer (&optional prefix)
  "Check from `point-min'."
  (my-lint-layout-point-min
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
    (while (re-search-forward "^[ \t]*[$][a-zA-Z0-9_>. \t-]+=[^=]" nil t)
      (setq col (- (current-column) 2))
      (unless (my-lint-layout-php-test-line-up-p col)
	(my-lint-layout-message
	 (format "[assignment] Assignment(=) at col %d possibly not lined-up"
		 col)
	 (my-lint-layout-current-line-number)
	 prefix))
      (forward-line 1))))

;;; ............................................................. &doc ...

(defsubst my-lint-layout-php-doc-string-narrowed-current-line-number ()
  "Return correct line number in narrowed doc-block."
  ;;  Because of narrowing, the first line is not 1, but 0.
  (+ line (1- (my-lint-layout-current-line-number))))

(defun my-lint-layout-php-doc-string-test-function
  (str line &optional prefix data type)
  "docstring is in STR, at LINE number. PREFIX for messages.
The DATA is function content string."
  (let ((class-p (my-lint-layout-save-point
		  (my-lint-layout-search-backward-class-p)))
	(need-return-p
	 (and data
	      (string-match "^[ \t]*return\\>[ \t]*[^; \t\r\n]" data)))
	(need-param-p
	 (and data
	      (string-match
	       (concat
		my-lint-layout-php-function-regexp
		"[ \t]*[^) \t\r\n]")
	       data)))
	(param  (string-match "@param" str))
	(access (string-match "@access" str))
	return)
    (when (string-match "this[ \t]+\\(function\\|method\\)" str)
      (my-lint-layout-message
       (format "[phpdoc] Unnecessary wording: %s" (match-string 0 str))
       line prefix))
    (when (and class-p
	       (not access))
      (my-lint-layout-message
       "[phpdoc] @access token not found"
       line prefix))
    (when (and need-param-p
	       (not param))
      (my-lint-layout-message
       "[phpdoc] @param token not found"
       line prefix))
    (when (and param
	       (not need-param-p))
      (my-lint-layout-message
       "[phpdoc] @param token is unnecessary"
       line prefix))
    (when (and return
	       (not need-return-p))
      (my-lint-layout-message
       "[phpdoc] @return token is unnecessary"
       line prefix))
    (when (and need-return-p
	       (not (setq return (string-match "@return" str))))
      (my-lint-layout-message
       "[phpdoc] @return token not found"
       line prefix))
    (if (and (and access param)
	     (> access param))
	(my-lint-layout-message
	 "[phpdoc] incorrect order. Should be @access..@param"
	 line prefix))
    (if (and (and access return)
	     (> access return))
	(my-lint-layout-message
	 "[phpdoc] Incorrect order. Should be @access..@return"
	 line prefix))))

(defun my-lint-layout-php-doc-examine-content-function
  (str line &optional prefix data)
  "Examine content: function. Expects narrow to docstring.
STR is docstring at LINE number. PREFIX is for messages.
DATA is the full function content."
  (save-excursion
    ;;  * @param  $var string
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
	       (+ line (my-lint-layout-current-line-number))
	       prefix))
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
	       (+ line (my-lint-layout-current-line-number))
	       prefix)))))))))

(defun my-lint-layout-php-doc-string-test-var-class (str line &optional prefix)
  "Examine dostring: variable."
  (let ((access-p (string-match "@access" str))
	(var-p    (string-match "@var" str)))
    (unless access-p
      (my-lint-layout-message
       "[phpdoc] @access token not found"
       line
       prefix))
    (unless var-p
      (my-lint-layout-message
       "[phpdoc] @var token not found"
       line
       prefix))
    (when (and access-p
	       var-p
	       (> access-p var-p))
      (my-lint-layout-message
       "[phpdoc] Incorrect order. Should be @access..@var"
       line
       prefix))))

(defun my-lint-layout-php-doc-string-test-var-global (str line &optional prefix)
  "Examine dostring: variable."
  (when (string-match "[*][ \t]*\\(@[a-z][a-z][a-z].*\\)" str)
    (my-lint-layout-message
     (format "[phpdoc] Possibly misplaced token: %s" (match-string 1))
     line
     prefix)))

(defun my-lint-layout-php-doc-string-test-class (str line &optional prefix)
  "Examine dostring: class."
  (unless (string-match "@package" str)
    (my-lint-layout-message
     "[phpdoc] @package token not found"
     line
     prefix)))

(defun my-lint-layout-php-doc-examine-content-other--test-doc-comment
  (line &optional type prefix)
  "Check doc-comment."
    (unless (looking-at "^[ \t]+[*]")
      (my-lint-layout-message
       "[phpdoc] Not a valid documentation comment"
       (1+ line)
       prefix)))

(defun my-lint-layout-php-doc-examine-content-other--test-period
  (line &optional type prefix)
  "Check that line ends top period."
    ;;  Complete sentence ends to period.
    (unless (looking-at "^[ \t]+[*].*\\.")
      (my-lint-layout-message
       (format
	"[phpdoc] Line is not a complete sentence ending to period(.)%s"
	(if (memq 'include type)
	    " (interpreted as require or include comment)"
	  ""))
       (1+ line)
       prefix)))

(defun my-lint-layout-php-doc-examine-content-other--first-sentence
  (line &optional type prefix)
  "Check two words, that t1st line is sentence."
  (when (and (not
	      (looking-at
	       (concat ".*"
		       my-lint-layout-generic-doc-1st-line-ignore-regexp)))
	     (not (looking-at
		   "^[ \t]+[*][ \t]*[^ \t\r\n]+[ \t][^ \t\r\n]+")))
    ;; Search at least two words. Ignore toplevel comment
    (when (not (memq 'file type))
      (my-lint-layout-message
       (format "[phpdoc] Line does not explain code that follows%s"
	       (if (memq 'include type)
		   " (interpreted as require or include comment)"
		 ""))
       (1+ line)
       prefix))))

(defun my-lint-layout-php-doc-examine-content-other--first-capital
  (line &optional type prefix)
  "Check first line capital letter."
  (my-lint-layout-with-case
    (unless (looking-at "^[ \t]+[*][ \t]*[A-Z]")
      (my-lint-layout-message
       (format "[phpdoc] Sentence does not start with capital letter%s"
	       (if (memq 'include type)
		   " (interpreted as require include comment)"
		 ""))
       (1+ line)
       prefix))))

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
	 "[phpdoc] Near *-character; text is not indented with spaces"
	 (1+ line)
	 prefix))))

(defun my-lint-layout-php-doc-examine-content-other--first-separator
  (line &optional type prefix)
  "Check that first line is sperated by one empty line.
/**
 *  First Line. Short description.
 *  <empty line>
 *  Long description.
 */"
  (my-lint-layout-with-case
    (unless (looking-at "^[ \t]*[*][ \t]*$")
      (my-lint-layout-message
       "[phpdoc] No empty line after first line short description"
       (+ 2 line)
       prefix)))
  (goto-char (point-min)))

(defun my-lint-layout-php-doc-examine-content-other--empty-line-tokens
  (line &optional type prefix)
  "Check empty line before @-tokens."
  (when (re-search-forward "^[ \t]*[*][ \t]@" nil t)
    (forward-line -1)
    (unless (looking-at "^[ \t]*[*][ \t]*$")
      (my-lint-layout-message
       "[phpdoc] no empty line before starting @-token"
       (+ line (my-lint-layout-current-line-number))
       prefix))))

(defun my-lint-layout-php-doc-examine-content-other--indent-text
  (line &optional prefix)
  "Check proper indent."
  (when (my-lint-layout-doc-line-indent-p)
    (let ((text (match-string 2)))
      (unless (string= text text-indent)
	(my-lint-layout-message
	 "[phpdoc] Text indentation mismatch: lined-upnot same as above."
         (my-lint-layout-php-doc-string-narrowed-current-line-number)
	 prefix)))))

(defun my-lint-layout-php-doc-examine-content-other--indent-col-error
  (col line &optional prefix)
  "Write error:  *-character is not lined up at COL."
  (my-lint-layout-message
   (format "[phpdoc] *-character does not start at column %d" col)
   line
   prefix))

(defun my-lint-layout-php-doc-examine-content-other--indent-col
  (col line &optional prefix)
  "Check that *-character is lined up at COL.
Write error at LINE with PREFIX."
  (let (point)
    (when (setq point (my-lint-layout-doc-line-indent-p))
      (my-lint-layout-save-point
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
   (format "[phpdoc] /** line contains extra characters%s" (or string ""))
   line
   prefix))

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
    (my-lint-layout-save-point
      (my-lint-layout-php-doc-examine-content-other--all-lines line type prefix))
    (forward-line 1)
    (my-lint-layout-php-doc-examine-content-other--test-doc-comment line type prefix)
    (my-lint-layout-php-doc-examine-content-other--test-period line type prefix)
    (unless (memq 'file type)
      (my-lint-layout-php-doc-examine-content-other--first-sentence line type prefix)
      (my-lint-layout-php-doc-examine-content-other--first-capital line type prefix))
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
    (if (my-lint-layout-type-class-variable-dollar-string-p str)
	(push 'var type))
    (if (my-lint-layout-type-class-string-p str)
	(push 'class type))
    (if (my-lint-layout-type-function-string-p str)
	(push 'function type))
    (if (my-lint-layout-type-include-string-p str)
	(push 'include type))
    (if (my-lint-layout-type-variable-string-p str)
	(push 'var-global type))
    type))

(defun my-lint-layout-php-function-region-at-point ()
  "Return function '(beg end) points with indentation.
Point must be at function start line."
  (save-excursion
    (goto-char (line-beginning-position))
    (let (indent
	  col
	  beg)
    (when (looking-at my-lint-layout-php-function-regexp)
      (setq beg (point))
      (setq indent (match-string 1))
      (goto-char (match-beginning 1))
      (setq col (current-column))
      ;; FIXME: We rely on indentation to close the function
      (when (re-search-forward (concat "^" indent "}") nil t)
	(list beg (point)))))))

(defun my-lint-layout-php-function-string-at-point ()
  "Return function string if any at point."
  (multiple-value-bind (beg end)
      (my-lint-layout-php-function-region-at-point)
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
	  (setq data (my-lint-layout-php-function-string-at-point)))
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
	    (my-lint-layout-php-doc-string-test-function str line prefix data)
	    (my-lint-layout-php-doc-examine-content-function
	     str line prefix data)))
	  (my-lint-layout-php-doc-examine-content-other
	   str line type prefix))))))

(defun my-lint-layout-php-check-doc--test-empty-line-above (&optional message)
  "Check empty line before doc-block."
  (my-lint-layout-save-point
    (forward-line -1)
    (unless (looking-at "^[ \t]*[{<]\\|^[ \t\r]*$")
      ;; private $var;
      ;; /**
      ;;  * Documentation
      ;;  */
      (my-lint-layout-message
       "[newline] No empty line before documentation block."
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-php-check-doc-main (&optional prefix)
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
  (let (line
	point
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
	    top-level-p (my-lint-layout-save-point
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
		    (my-lint-layout-php-doc-examine-typeof
		     (my-lint-layout-current-line-string)))
	      (if (and (not type)
		       valid-p)
		  (setq type '(file)))
	      end)
	(setq str  (buffer-substring beg end)
	      line (my-lint-layout-current-line-number))
	(unless next-line-valid-p
	  (my-lint-layout-message
	   "[phpdoc] format layout error"
	   line prefix))
	(cond
	 ;; (my-lint-layout-doc-package-string-p str)) ;Skip
	 ;; (my-lint-layout-doc-var-string-p str)) ;Skip
	 ((not valid-p)
	  (my-lint-layout-message
	   (concat
	    "[phpdoc] Possibly misplaced. "
	    "Expected class, function, variable, require or include")
	   line prefix))
	 (t
	  (let ((top-level-p (my-lint-layout-doc-package-string-p str)))
	    (unless top-level-p
	      (my-lint-layout-php-doc-examine-main
	       beg
	       end
	       type
	       line
	       prefix)))))))))

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
	(message "No file information found at current line.")
      (let ((buffer (my-lint-output-mode-find-buffer file)))
	(if (not buffer)
	    (message "Can't find buffer for '%s'" file)
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

;;; ................................................. &php-interactive ...

(defun my-lint-layout-php-check-all-tests (&optional prefix)
  "Run `my-lint-layout-check-php-generic-functions'."
  (my-lint-layout-run-list
   my-lint-layout-check-php-generic-functions prefix))

(defun my-lint-layout-php-check-code-run (&optional point prefix)
  (my-lint-layout-run-list
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
  (my-lint-layout-run-list
   my-lint-layout-check-php-doc-functions
   prefix
   point))

(defun my-lint-layout-php-check-phpdoc-interactive (&optional point prefix erase)
  "Run `my-lint-layout-check-php-doc-functions' from current POINT forward."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-php-check-phpdoc-run point prefix)))

(defun my-lint-layout-php-check-all-interactive (&optional point prefix)
  "Run All PHP checks."
  (interactive)
  (my-lint-with-result-buffer 'erase 'display
    (my-lint-layout-php-check-all-tests prefix)))

(defun my-lint-layout-check-generic-interactive (&optional prefix)
  "Run check according to file extension: *.php, *.css, *.php."
  (interactive)
  (let ((name (buffer-name)))
    (cond
     ((string-match "\\.php" name)
      (my-lint-layout-php-check-all-interactive (point-min) name))
     ((string-match "\\.css" name)
      (my-lint-layout-css-check-buffer-interactive name))
     ((string-match "\\.sql" name)
      (my-lint-layout-sql-buffer-interactive name))
     (t
      (message "no checks defined for: %s" name)))))

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
	  (insert-file-contents file)
	  (my-lint-layout-run-list
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
   ((string-match "\\.css$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-css-check-batch-all))
   ((string-match "\\.sql$" file)
    (my-lint-layout-check-file-list
     file
     'my-lint-layout-sql-check-batch-all))
   (t
    (message "[WARN] No checks defined for '%s'" file))))

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

;; Selectively run test for list of files
(defun my-lint-layout-check-batch-command-line (&optional function)
  "Run FUNCTION (or list of) over files on command line."
  (my-lint-layout-check-batch-file-list
   command-line-args-left function)
  (my-lint-layout-princ-results))

(defun my-lint-layout-check-batch-generic-command-line ()
  "Run correct check for each type of file on command line."
  (let ((debug-on-error t))
    (dolist (file command-line-args-left)
      (my-lint-layout-check-generic-file file))
    (my-lint-layout-princ-results)))

;;      (message
;;       (replace-regexp-in-string "%" "%%" (buffer-string)))))))

;; End of file
