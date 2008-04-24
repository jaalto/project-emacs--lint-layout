;; php-layout.el -- Utilities to check PHP code layout
;;
;; Copyright (C) 2006-2007 Jari Aalto
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
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details at <http://www.gnu.org/copyleft/gpl.html>
;;
;; All *check-* function are callable.
;; Look results in `my-layout-changelog-check-main' *Layout checks*"

(require 'regexp-opt)

(eval-when-compile
  (require 'cl))

(defconst my-lint-layout-buffer-name "*Layout checks*"
  "*Buffer name for results.")

(defconst my-lint-layout-generic-line-length-max 80
  "*Maximum line length.")

(defconst my-lint-layout-generic-indent-step 4
  "*Indent step.")

(defconst my-lint-layout-generic-access-modifier-regexp
  (concat
   "\\<"
   (regexp-opt
    '("abstract"
      "public"
      "protected"
      "private"
      "static")
    t)
   "\\>")
  "Access modifiers.")

(defconst my-lint-layout-php-function-regexp
  (concat
   "^\\([ \t]*\\)"
   "\\(?:"
   my-lint-layout-generic-access-modifier-regexp
   "\\)?"
   "[ \t]*"
   "\\<function\\>[ \t]+")
 "Function regexp. Submatch 1: Indent")

(defconst my-lint-layout-php-doc-location-regexp
   (concat
    "^[ \t]*"
    "\\("
    my-lint-layout-generic-access-modifier-regexp
    "\\|\\<"
    (regexp-opt
     '("function"
       "require"
       "include")
     t)
    "\\>\\)")
  "Location, where documentation should exist.")

(defconst my-lint-layout-generic-control-statement-start-regexp
  (concat
   "\\<"
   (regexp-opt
    '("if"
      "while"
      "for"
      "foreach"
      "try")
    t)
   "\\>")
  "Control statement keyword regexp.")

(defconst my-lint-layout-generic-control-statement-continue-regexp
  (concat
   "\\<"
   (regexp-opt
    '("else"
      "elsif"
      "elseif"
      "else[ \t]+if"
      "catch")
    t)
   "\\>")
  "Control statement continue keyword regexp.")

(defconst my-lint-layout-generic-control-statement-regexp
  (concat
   my-lint-layout-generic-control-statement-start-regexp
   "\\|"
   my-lint-layout-generic-control-statement-continue-regexp)
  "Control statement keyword.")

(defconst my-lint-layout-generic-xml-tag-regexp
  "<[?]\\|[?]>"
  "xml tag: starting, closing.")

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

(defconst my-php-layout-brace-and-code-regexp
  "[}][ \t]*\r?\n[ \t]*\\([^{} \t\r\n]+\\)"
  "Match brace end } followed by immediate code.")

;;; ....................................................... &utilities ...

(put 'my-lint-layout-with-result-buffer 'lisp-indent-function 0)
(defmacro my-lint-layout-with-result-buffer (&rest body)
  "Run body in `my-lint-layout-buffer-name'."
  `(let ((buffer (get-buffer-create my-lint-layout-buffer-name)))
     (with-current-buffer buffer
       ,@body)))

(put 'my-lint-layout-with-case 'lisp-indent-function 0)
(defmacro my-lint-layout-with-case (&rest body)
  "Run BODY with `case-fold-search' set to nil."
  `(let (case-fold-search)
     ,@body))

(put 'my-lint-layout-point-min 'lisp-indent-function 0)
(defmacro my-lint-layout-point-min (&rest body)
  "Run BODY with from `point-min'."
  `(save-excursion
    (goto-char (point-min))
    ,@body))

(put 'my-lint-layout-flet-run-at-point 'lisp-indent-function 0)
(defmacro my-lint-layout-flet-run-at-point (&rest body)
  "DEfine function `run' which preserves point. Run BODY."
  (let ((point (make-symbol "--point--")))
    `(let ((,point (point)))
       (flet ((run (func &rest args)
		   (goto-char ,point)
		   (apply func args)))
	 ,@body))))

(put 'my-lint-layout-with-interactive 'lisp-indent-function 0)
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
  ;;  - The count-lines returns 0 for 1st line --> 1+
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

(defsubst my-lint-layout-doc-package-string-p (str)
  "Check @package phpdoc"
  (string-match "@package\\|@copyright\\|@author\\|@version" str))

(defsubst my-lint-layout-doc-var-string-p (str)
  "Check @package phpdoc"
  (string-match "@var" str))

(defsubst my-lint-layout-looking-at-doc-p ()
  (and (looking-at my-lint-layout-generic-doc-line-regexp)
       (match-beginning 0)))

(defsubst my-lint-layout-search-doc-beginning ()
  (and (re-search-forward "^[ \t]*/[*][*][ \t\r]*" nil t)
       (match-beginning 0)))

(defsubst my-lint-layout-search-doc-end ()
  (and (re-search-forward "[*]/" nil t)
       (match-end 0)))

(defsubst my-php-layout-search-function-beginning ()
  (re-search-forward my-lint-layout-php-function-regexp nil t))

(defsubst my-lint-layout-search-class-start ()
  (and (re-search-forward
	"^[ \t]*\\(?:\\(abstract\\)[ \t]*\\)?class\\>[ \t]"
	nil t)
       (match-end 0)))

(defsubst my-lint-layout-search-interface-start ()
  (and (re-search-forward
	"^[ \t]*interface\\>[ \t]"
	nil t)
       (match-end 0)))

(defsubst my-lint-layout-type-statement-string-p (str)
  (string-match ";[ \t]*$" str))

;; For example: "private $var;"
(defsubst my-lint-layout-type-variable-string-p (str)
  (string-match
   (concat
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

(defsubst my-lint-layout-looking-at-doc-end-valid-p ()
  "Check that */ is followed by a function or variable definition."
  (and
   (looking-at
    (concat
     "[ \t]*\\<\\("
     "class\\|function\\|public\\|protected\\|private"
     "\\|static\\|var"
     "\\|require\\|include"
     "\\)\\>"))
   (match-string 1)))

(defsubst my-lint-layout-looking-at-comment-p (str)
  "Check if STR looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\)" str))

(defsubst my-lint-layout-looking-at-comment-line-p ()
  "Check if line looks like comment."
  (string-match "^[ \t]*\\([*]\\|//\\)"
		(my-lint-layout-current-line-string)))



(defsubst my-lint-layout-looking-at-assignment-column-p ()
  "Return assignmnet '=' column position."
  ;;  Do not count equal '=='
  (when (looking-at "^[ \]*[$a-z0-9_]+[^=\r\n]*=[^=]")
    (- (length (match-string 0)) 2)))

(defsubst  my-lint-layout-make-result-header-string ()
  "Return file and time string."
  (format "== %s %s\n"
	  (if buffer-file-name
	      buffer-file-name
	    (buffer-name))
	  (format-time-string "%Y-%m-%d %H:%M")))

(defsubst my-lint-layout-result-header-string-insert ()
  "Insert file and time string."
  (insert (my-php-layout-result-buffer-header)))

(defsubst my-lint-layout-result-erase-buffer ()
  "Create and clear `my-lint-layout-buffer-name'."
  (my-lint-layout-with-result-buffer
    (erase-buffer)))

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

;;; ........................................................... &occur ...

(defconst my-php-layout-check-regexp-occur-modern-style-list
  (list
   '("[a-z].*{[ \t\r\n]*$"
     "Possibly K&R brace style, expected line-up")
   '("[a-z].*}[ \t\r\n]*$"
     "Possibly K&R brace style, expected line-up")
   '("^[ \t]*function[ \t]+[a-z0-9_]+("
     "In funcdef, no space before starting paren")
   '("^[ \t]*[$][a-z0-9]+_[a-z0-9]+[ \t\r\n]*="
     "variable name not CamelCase")
   '("^[ \t]*function[ \t][a-z0-9]+_[^ \t]*[ \t]*("
     "In funcdef, name not CamelCase"))
  "Check Modern layout style.")

;; NOTES:
;; *) It's okay to use "@" suppression
;;
;;  $this->conn = @mysql_connect(DBHOST, DBUSER, DBPASS);
;;  if ( ! $this->conn )

(defconst my-php-layout-check-regexp-occur-list
  (list

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
   '("^[ \t]*var[ \t]"
     "Old vardef. Migrate to syntax public|protected: ")
   '("\\<ereg[_a-z]*(.*)"
     "preg*() function family recommended for")
   '("\\<include[_a-z][( \t]*[\"\'$]"
     "require*() function family is safer than")

   '("\\<echo[( \t]*[\"\'$]"
     "Standard print() recommended for")

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
     "In statement, no space after starting paren: ")

   '("\\<\\(if\\|else\\|foreach\\|for\\|while\\)[ \t]*([^ $)\t\r\n]"
     "In statement, no space after keyword and paren")

   '("\\<\\(if\\|foreach\\|while\\)[ \t]*(.*[^ \t])[ \t]*$"
     "In statement, no space before closing paren")

   '("this->[^][ )\t\r\n]+[ \t]("
     "In funcall, possibly extra space before opening paren")

   ;; code );
   '("[a-z][_a-z0-9]+([^)]*[ \t]+)\\|[^) \t\r\n]+[ \t]);"
     "In funcall, possibly extra space before closing paren"
     "\\<\\(if\\|foreach\\|while\\|assert\\)")

   ;; function ( param, def)
   '("^[ \t]*function[ \t]*.*([ \t]"
     "In funcdef, extra space after starting paren")
   ;; function (param, def )
   '("^[ \t]*function[ \t]*.*([^)\r\n]*[ \t])"
     "In funcdef, extra space before closing paren")

   (list
    (concat
     "[$][a-z0-9_>-]+[.][$\"][^);]"
     "\\|\"[.][$][a-z_]"
     "\\|\"[.][a-z_0-9]+"
     "\\|[)][.][\"][^);]")
    "No surrounding spaces aroud concat(.)")

   ;; if ( $query and mysql_result == false )
   (list
    (concat
     "\\<\\(?:elseif\\|if\\|foreach\\|while\\)[ \t]*("
     ".*[ \t][$][^ 0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|and\\|or\\)[ \t]+"
     "[a-z0-9_]+[) \t\r\n]")
    "Possibly missing vardef($) in relational test at right")

   ;; if ( value and $var )
   (list
    (concat
     "\\<\\(?:elseif\\|if\\|foreach\\|while\\)[ \t]*("
     ".*[ \t][^ 0-9\t\r\n]+\\>"
     "[ \t]*\\(?:&&\\|[|][|]\\|and\\|or\\)[ \t]+"
     "[$][a-z0-9_]+[) \t\r\n]")
    "Possibly missing vardef($) in relational test at left")

   '("[$][a-z][_a-z0-9]*=[ \t]+[$a-z_0-9\"\']"
     "no space at left of equal sign")
   '("[$][a-z][_a-z0-9]*[ \t]+=[$a-z_0-9\"']"
     "no space at right of equal sign")

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
     "Possibly mispelled __(de|con)structor"))
  "Search ((REGEXP MESSAGE [NOT-REGEXP] [FUNC]) ..).")

(defun my-php-layout-check-regexp-occur (&optional prefix list)
  "Check regepx in LIST or `my-php-layout-check-regexp-occur-list'."
  (let (line)
    (dolist (elt (or list
		     my-php-layout-check-regexp-occur-list))
      (multiple-value-bind (re msg not-re func) elt
	(save-excursion
	  (while (re-search-forward re nil 'noerr)
	    (setq line (my-lint-layout-current-line-string))
	    (when (and (not (my-lint-layout-looking-at-comment-p line))
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

(defun my-php-layout-check-regexp-occur-main (&optional prefix)
  "Run all occur checks."
  (my-lint-layout-flet-run-at-point
    (run 'my-php-layout-check-regexp-occur prefix)
    (run 'my-php-layout-check-regexp-occur
	 prefix
	 my-php-layout-check-regexp-occur-modern-style-list)))

(defun my-php-layout-check-regexp-occur-buffer (&optional prefix)
  "Check from `point-min' with `my-php-layout-check-regexp-occur'."
  (my-lint-layout-point-min
    (my-php-layout-check-regexp-occur-main prefix)))

(defun my-php-layout-check-regexp-occur-buffer-interactive ()
  "Call `my-php-layout-check-regexp-occur-buffer'."
  (interactive)
  (my-lint-layout-with-interactive
    (my-php-layout-check-regexp-occur-buffer)))

;;; ............................................................ &misc ...

(defun my-lint-layout-generic-class-forward ()
  "Goto next class definition."
  (let ((class  (save-excursion (my-lint-layout-search-class-start)))
	(iface  (my-lint-layout-search-interface-start)))
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


;;; ........................................................... &print ...

(defun my-php-layout-check-multiple-print (&optional prefix)
  "Check multiple print statements."
  (save-excursion
    (while (re-search-forward
	    ;;  3 x threshold
	    "^[ \t]*print.*\n[ \t]*print.*\n[ \t]*print" nil t)
      (my-lint-layout-message
       "Multiple print*() calls. Possible alternative: HERE syntax"
       (my-lint-layout-current-line-number)
       prefix))))

(defsubst my-php-layout-print-command-forward-1 ()
  "Find print or echo command."
  (re-search-forward
   ;; text/html; charset=utf-8
   "^[ \t]*\\(print\\|echo\\)[ \t]*[(\"][^;]+;" nil 'noerr))

(defsubst my-php-layout-print-command-forward ()
  "Search print or echo command. Return beginning point of match."
  (let (beg)
    (when (my-php-layout-print-command-forward-1)
      (setq beg (match-beginning 0))
      (unless (looking-at "[; \t]*$")
	(when (re-search-forward ";[ \t]*$" nil 'noerr)))
      beg)))

(defun my-php-layout-check-multiline-print (&optional prefix)
  "Check long print statements, when there is no $var anywhere.

print 'this' .
      'and' .
      ....
      ;
"
  (let (beg
	str
	lines)
    (while (setq beg (my-php-layout-print-command-forward))
      (setq str (buffer-substring beg (point)))
      (unless (string-match "[$]" str) ;No variables used
	(setq lines (my-lint-layout-count-lines-in-string str))
	(when (> lines 3)
	  (my-lint-layout-message
	   "Maintenance problem, better use HERE doc syntax (<<<)"
	   (- (my-lint-layout-current-line-number) lines)
	   prefix))))))

;;; ......................................................... &php-sql ...

(defsubst my-php-layout-var-forward-1 ()
  "Find variable definition."
  (re-search-forward "^[ \t]*\\([$][a-z].*=[^;]+\\)" nil 'noerr))

(defsubst my-php-layout-var-sql-forward-1 ()
  "Find SQL content in variable.
Point is at end of variable after search.
Return variable content string."
  (let (ret
	point
	str)
    (while (and (null ret)
		(my-php-layout-var-forward-1))
      (setq point (match-beginning 0)
	    str   (match-string 0))
      (when (or (string-match "\\<INSERT[ \t\r\n]+INTO\\>" str)
		(and (string-match "\\<SELECT[ \t\"'$]" str)
		     (string-match "\\<FROM[ \t\"'$]" str)))
	(setq ret str)))
    str))

(defun my-php-layout-check-multiline-sql (&optional prefix)
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
    (while (setq str (my-php-layout-var-sql-forward-1))
      (when (and (string-match "^\\([ \t]*\".*[\r\n]\\)+" str)
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
  (let (tag)
    (while (re-search-forward my-lint-layout-generic-xml-tag-regexp nil t)
      (setq tag (match-string 0))
      (when (string= tag "<?")
	(unless (looking-at tag)
	  (my-lint-layout-message
	   (format "Unknown opening xml tag. Expected <?%s: %s"
		   tag
		   (my-lint-layout-current-line-string))
	   (my-lint-layout-current-line-number)
	   prefix))))))

(defun my-php-layout-check-xml-tags-lazy (&optional prefix)
  "Check <?php tag."
  (my-lint-layout-check-xml-tags-lazy "php"))

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

(defun my-php-layout-check-control-statements (&optional prefix)
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

(defun my-php-layout-check-block-end-and-code (&optional prefix)
  "Block end followed by code immediately after.

	}
	return;
"
  (let (line
	str)
    (while (re-search-forward my-php-layout-brace-and-code-regexp nil t)
      (setq str (buffer-substring (match-beginning 1) (match-end 1)))
      (when (and
	     (not (my-lint-layout-looking-at-comment-p str))
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

(defun my-php-layout-check-comment-statements (&optional prefix)
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

(defun my-php-layout-doc-above-p ()
  "Check if phpdoc is in above line."
  (save-excursion
    (goto-char (line-beginning-position))
    (skip-chars-backward "  \t\r\n")
    (goto-char (line-beginning-position))
    ;; FIXME: Not bullet proof
    (looking-at "^[ \t]*[*]/[ \t]*$")))

(defun my-php-layout-check-doc-missing (&optional prefix)
  "Check missing documentation."
  (let (class-p
	str
	line)
    (save-excursion
      (setq class-p (or (my-lint-layout-search-class-start)
			(my-lint-layout-search-interface-start))))
    (while (re-search-forward
	    my-lint-layout-php-doc-location-regexp
	    nil t)
      (unless (my-php-layout-doc-above-p)
	(setq str  (my-lint-layout-current-line-string)
	      line (my-lint-layout-current-line-number))
	(cond
	 ((and class-p
	       (my-lint-layout-type-include-string-p str))
	  (my-lint-layout-message
	   "[phpdoc] require or include not documented"
	   line prefix))
	 ((my-lint-layout-type-function-string-p str)
	  (my-lint-layout-message
	   "[phpdoc] function not documented"
	   line prefix))
	 ;; Skip "function files"
	 ((my-lint-layout-type-include-string-p str))
	 ((my-lint-layout-type-statement-string-p str)
	  (my-lint-layout-message
	   "[phpdoc] variable not documented"
	   line prefix)))))))

(defsubst my-php-layout-indent-level (str)
  "Count indent."
  (and str
       (setq str (replace-regexp-in-string "\t" "        " str))
       (string-match "^[ \t]*" str)
       (length (match-string 0 str))))

(defun my-php-layout-check-indent-string-check (str line &optional prefix base-indent)
  "Check STR for correct indent and report LINE as error."
  (let ((i (my-php-layout-indent-level str))
	(istep my-lint-layout-generic-indent-step))
    (when (numberp i)
      (cond
       ((and (or (null base-indent)
		 (< i (+ base-indent istep)))
	     (not (zerop (mod i 4))))
	(my-lint-layout-message
	 (format "[code] Possibly incorrect indent at col %d where multiple of %d expected"
		 i
		 istep)
	 (my-lint-layout-current-line-number)
	 prefix))
       ((and base-indent
	     (not (zerop i))
	     (eq i base-indent))
	(my-lint-layout-message
	 (format "[code] Possibly missing indentation at column %d" i)
	 (my-lint-layout-current-line-number)
	 prefix))))))

(defsubst my-php-layout-statement-brace-forward (&optional brace)
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


(defsubst my-php-layout-statement-brace-end-forward (&optional col)
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

(defun my-php-layout-statement-brace-block-check (beg end &optional base-indent prefix)
  "Check brace block between BEG and END using BASE-INDENT"
  (or base-indent
      (setq base-indent (current-column)))
  (goto-char beg)
  (let (indent
	str)
    (while (re-search-forward "^\\([ \t]*\\)\\([^ \t\r\n]+\\)" end 'noerr)
      (setq indent (match-string 1)
	    str    (match-string 2))
      (my-php-layout-check-indent-string-check indent str prefix base-indent))))

(defun my-php-layout-check-statement-start (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
	 (re (concat "^"
		     "\\([ \t]*\\)"
		     "\\("
		     my-lint-layout-generic-control-statement-regexp
		     "\\)[ \t{]*$"))
	 str
	 point
	 point2
	 indent
	 brace-p
	 comment-p
	 continue-p
	 statement-start-col
	 statement-line
	 brace-start-col
	 brace-start-line
	 brace-end-col
	 brace-end-point
	 brace-end-line)
    (while (re-search-forward re nil t)
      (setq point  (match-beginning 1)
	    point2 (match-beginning 2)
            indent (match-string 1)
	    str    (match-string 2)
	    continue-p (string-match
			my-lint-layout-generic-control-statement-continue-regexp
			str))
      (save-excursion
	(goto-char point2)
	(setq statement-start-col (current-column)
	      statement-line (my-lint-layout-current-line-number))
	;; if ()
	;;    i=0;
	;;
	;; if ( $a and
	;;      $b )
	;; {
	(unless (setq brace-p (looking-at ".*{"))
	  ;; Peek next line
	  (forward-line 1)
	  (setq brace-p (not (looking-at ".*;"))))

	;; Peek comment above
	(when continue-p
	  (goto-char point)
	  (goto-char (line-beginning-position))
	  (skip-chars-backward " \t\r\n")
	  (if (my-lint-layout-looking-at-comment-line-p)
	      (my-lint-layout-message
	       (format "Misplaced comment. Should be inside '%s' block" str)
	       (my-lint-layout-current-line-number)
	       prefix)))
	)
      (my-php-layout-check-indent-string-check indent statement-line prefix)
      (when (string-match
	     my-lint-layout-generic-control-statement-continue-regexp
	     str)
	(save-excursion
	  (forward-line -1)
	  (unless (looking-at "^[ \t]*}")
	    (my-lint-layout-message
	     (format "keyword '%s' is not attached to above brace block"
		    str)
	    statement-line
	    prefix))))
      (cond
       ((null brace-p)
	(my-lint-layout-message
	 (format "brace block not found near keyword '%s' at col %s"
		 str
		 statement-start-col)
	 statement-line
	 prefix))
       (t
	(my-php-layout-statement-brace-forward)
	(setq brace-start-col  (current-column)
	      brace-start-line (my-lint-layout-current-line-number))
	(save-excursion
	  (let ((beg (line-end-position)))
	    (when (setq brace-end-point
			(my-php-layout-statement-brace-end-forward))
	      (my-php-layout-statement-brace-block-check
	       beg
	       brace-end-point
	       (current-column)
	       prefix))))
	(unless (eq statement-start-col brace-start-col)
	  (my-lint-layout-message
	   (format "brace { not directly under keyword '%s', expect col %d"
		   str
		   statement-start-col)
	   (my-lint-layout-current-line-number)
	   prefix)))))))

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

(defun my-php-layout-check-vc-conflict-marker (&optional prefix)
  "Check version control conflict markers marker."
  (save-excursion
    (while (re-search-forward
	    my-lint-layout-vc-conflict-marker-regexp
	    nil
	    'noerr)
      (my-lint-layout-message
       (format "[misc] Possible unresolved conflict: %s"
	       (my-lint-layout-current-line-string))
       (my-lint-layout-current-line-number)
       prefix))))

;;; ........................................................ &spelling ...

(defconst my-lint-layout-word-search-regexp
  "Licence\\|Lisen[cs]e\
\\|This file is part of <program>\
\\|<program> is free software\
"
  "Search common misspelled or template words.")

(defun my-php-layout-check-words (&optional prefix)
  "Check words."
  (let (str)
    (while (re-search-forward
	    my-lint-layout-word-search-regexp
	    nil
	    'noerr)
      (setq str (match-string 0))
      (my-lint-layout-message
       (if (string-match "<" str)
	   (format "[misc] Possibly unfilled template: %s" str)
	 (format "[misc] Mispelled word: %s" str))
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-php-layout-check-words-buffer (&optional prefix)
  "Check from `point-min' with `my-php-layout-check-words'."
  (my-lint-layout-point-min
    (my-php-layout-check-words prefix)))

(defun my-php-layout-check-words-buffer-interactive ()
  "Run `my-php-layout-check-words-buffer' and show results."
  (interactive)
  (my-lint-layout-with-interactive
    (my-php-layout-check-words-buffer)))

;;; ............................................................. &eof ...

(defconst my-lint-layout-eof-regexp
  (regexp-quote "End of file")
  "End of file marker text.")

(defun my-lint-layout-check-eof-marker (&optional prefix)
  "Check EOF marker."
  (save-excursion
    (goto-char (point-max))
    (my-lint-layout-with-case
      (unless (re-search-backward
	       my-lint-layout-eof-regexp
	       (min (point-min)
		    (* 4 80))
	       'noerr)
	(my-lint-layout-message
	 (format "[misc] No exact EOF text found: '%s'"
		 my-lint-layout-eof-regexp)
	 (my-lint-layout-current-line-number)
	 prefix)))))

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
  (re-search-forward "^\n\\|[^\r\n]\n" nil 'noerr))

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

(defsubst my-php-layout-brace-forward-1 ()
  "Move to start brace {"
  ;; {
  ;;
  ;;    if ()
  (and (re-search-forward "[{][ \t]*\n[ \t]*\\(\r?\n[ \t\r\n]+\\)[^{}]" nil t)
       (list (point) 'beg)))

(defsubst my-php-layout-brace-forward-2 ()
  "Move to end brace }"
  ;;
  ;;
  ;; return
  ;;
  ;; }
  (and (re-search-forward "[^{][ \t]*\n\\([ \t]*\r?\n[ \t\r\n]*\\)[}]" nil t)
       (list (point) 'end)))

(defsubst my-php-layout-brace-forward-3 ()
  "Move to function brace."
  (and (re-search-forward
	(concat
	 "^[ \t]*\\([ \t\r\n]+\\)"
	 "\n[ \t]*\\(?:function\\|public\\|private\\|protected\\)")
	nil t)
       (list (point) 'function-before)))

(defsubst my-php-layout-brace-forward-4 ()
  "Move to empty brace {}"
  ;; if
  ;; {
  ;; }
  (and (re-search-forward "[{][ \t]*\r?\n[ \t]*[}]" nil t)
       (list (point) 'empty)))

(defsubst my-php-layout-brace-forward-5 ()
  "Move to empty block } + immediate code."
  (and (re-search-forward
	(concat "}" my-php-layout-brace-and-code-regexp)
	nil t)
       (list (point) 'end-brace-and-code)))

(defun my-php-layout-brace-forward-main ()
  "Move to first brace that contains problems."
  (let ((start (point))
	(point (point-max))
	ret-str
	str
	ret)
    (dolist (func '(my-php-layout-brace-forward-1
		    my-php-layout-brace-forward-2
		    my-php-layout-brace-forward-3
		    my-php-layout-brace-forward-4
		    my-php-layout-brace-forward-5))
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

(defun my-php-layout-brace-message (type line &optional count prefix)
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

(defun my-php-layout-check-brace-extra-newline (&optional prefix)
  "Check forward for extra newlines after '{' and before '}'"
  (let (status
	line
	str)
    (while (setq status (my-php-layout-brace-forward-main))
      (multiple-value-bind (type str) status
	(setq line (my-lint-layout-current-line-number))
	(my-php-layout-brace-message
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
     "[sql] non-standard backquote character"
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
	 (format "[sql] non-standard keyword: %s" (match-string 0 str))
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
	 (format "[sql] extra space found near size definition: %s"
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
	(when (string-match "NULL" str)	;; FIXME: NULL itself is not SQL92
	  (my-lint-layout-message
	   (format "[sql] non-standard NULL keyword: %s"
		   str)
	   (my-lint-layout-current-line-number)
	   prefix))))
      (when word
	(when (and (null non-std-kwd-p) ; Not yet checked
		   (not (string-match type-re word)))
	  (setq non-std-kwd-p t)
	  (my-lint-layout-message
	   (format "[sql] non-standard keyword or datatype: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	(when (and (null non-std-kwd-p)
		   (not (string-match type-sql92-re word)))
	  (my-lint-layout-message
	   (format "[sql] non-SQL92 may cause portability problems: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	;; FIXME: tests only datatype now
	(when (and (string-match sql-re word)
		   (my-lint-layout-with-case
		     (not (string-match sql-re word))))
	  (my-lint-layout-message
	   (format "[sql] keyword not in uppercase: %s" word)
	   (my-lint-layout-current-line-number)
	   prefix))
	(unless (string-match "^[ \t]+" str)
	  (my-lint-layout-message
	   (format "[sql] statement not indented (by %d)" step)
	   (my-lint-layout-current-line-number)
	   prefix))))))

(defun my-lint-layout-sql-create-table (table)
  "Check CREATE TABLE."
  (my-lint-layout-with-case
    (when (string-match "[A-Z]" table)
      (my-lint-layout-message
       (format "[SQL] portability problem, mixed case table name: %s" table)
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
	 "[sql] non-standard backquote character"
	 (my-lint-layout-current-line-number)
	 prefix))
      (when (string-match "[(]" line)
	(my-lint-layout-message
	 "[sql] misplaced starting paren (possibly not lined-up)"
	 (my-lint-layout-current-line-number)
	 prefix)))))

(defun my-lint-layout-sql-main (&optional prefix)
  "Check SQL syntax."
  (save-excursion
    (my-lint-layout-sql-check-create-table prefix))
  (save-excursion
    (my-lint-layout-sql-check-keywords prefix)))

;;; ............................................................. &css ...

(defsubst my-php-layout-check-multiple-statements (msg &optional prefix)
  "Check multiple ';'."
  (when (looking-at ".+;.*;")
    (my-lint-layout-message
     msg
     (my-lint-layout-current-line-number)
     prefix)))

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
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-css-attribute (&optional prefix)
  "Check attribute."
  (when (and (looking-at "\\(.*[a-z]\\):")
	     ;;  a:hover
	     (not (string-match "\\<a" (match-string 1)))
	     (looking-at "\\([^ \t\r\n]+[a-z]:\\([^ \t\r\n]+\\)\\)"))
    (my-lint-layout-message
     (format "[css] no space between colon and attribute value: %s" (match-string 1))
     (my-lint-layout-current-line-number)
     prefix)))

(defun my-lint-layout-css-color (&optional prefix)
  "Check color-attribute."
  (let (str)
    (when (and (looking-at ".*\\<color:[ \t]*#\\([a-f0-9]+\\)")
	       (setq str (match-string 1))
	       (not (eq 6 (length str))))
      (my-lint-layout-message
       (format "[css] color is not complete 6 digit hex value: %s"str)
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-css-body (&optional prefix)
  "Check body."
  (while (and (not (eobp))
	      (not (looking-at ".*[ \t][{}]")))
    (my-lint-layout-css-indent-level prefix)
    (my-lint-layout-css-attribute prefix)
    (my-lint-layout-css-color prefix)
    (my-php-layout-check-multiple-statements
     "[css] multiple attribute definitions (only one expected)")
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
	 "[css] no newline after token '{'"
	 line prefix))
      ((> lines 1)
	(my-lint-layout-message
	 (format "[css] extra %d empty lines after token '{'" (1- lines))
	 line prefix)))
      (my-lint-layout-current-line-string)
      (when (eq col 0)
	(my-lint-layout-message
	 (format "[css] not indented (by %d)"
		 my-lint-layout-generic-indent-step)
	 line prefix))
      ;;   background-color: #F8F8F8; border: 1px;
      (my-php-layout-check-multiple-statements
       "[css] multiple attribute definitions (only one expected)")
      (my-lint-layout-css-indent-level)
      (my-lint-layout-css-body prefix))))

(defun my-lint-layout-check-comment-javadoc-invalid (&optional prefix)
  "Check invalid Javadoc-style /** and @tag in comments."
  (let ((re1 (concat "^[ \t]*" (regexp-quote "/**")))
	(re2 "^[ \t]*[*][ \t]*@[a-z]"))
    (dolist (re (list re1 re2))
      (save-excursion
	(while (re-search-forward re nil 'noerr)
	  (my-lint-layout-message
	   (format "[css] Probably misplaced Javadoc/Phpdoc: %s"
		   (my-lint-layout-current-line-string))
	   (my-lint-layout-current-line-number)
	   prefix))))))

(defun my-lint-layout-css-check-main (&optional prefix)
  "Check Css"
  (my-lint-layout-flet-run-at-point
    (run 'my-lint-layout-check-comment-javadoc-invalid prefix)
    (run 'my-lint-layout-css-check-generic prefix)))

;;; ......................................................... &line-up ...

(defun my-php-layout-test-line-up-p (col)
  "Check current COL of '=' and next line."
  (let ((ret t)
	next)
    (when (and col
	       (> col 0))
      (save-excursion
	(move-to-column col)
	(if (not (looking-at ".*[$][a-zA-Z].*="))
	    ;;  var =
	    ;;      "is too big to include in line";
	    ;;
	    ;; Accept this as is
	    'no-variable-follows
	  (forward-line 1)
	  (if (setq next (my-lint-layout-looking-at-assignment-column-p))
	      (setq ret (eq col next))))))
    ret))

(defun my-php-layout-check-line-up-assignment (&optional prefix)
  "Check that '=' line up."
  (let (str
	col
	len
	line)
    (while (re-search-forward "^[ \t]*[$][a-zA-Z0-9_>. \t-]+=[^=]" nil t)
      (setq col      (- (current-column) 2)
	    line     (my-lint-layout-current-line-number))
      (unless (my-php-layout-test-line-up-p col)
	(my-lint-layout-message
	 (format "[assignment] token '=' is not lined-up at column %d" col)
	 line prefix))
      (forward-line 1))))

;;; ............................................................. &doc ...

(defun my-php-layout-doc-string-test-function
  (str line &optional prefix data)
  "docstring is in STR, at LINE number. PREFIX for messages.
The DATA is function content string."
  (let ((need-return-p
	 (and data
	      (string-match "^[ \t]*return\\>[ \t]*[^; \t\r\n]" data)))
	(need-param-p
	 (and data
	      (string-match
	       (concat
		my-lint-layout-php-function-regexp
		".*([ \t]*[^) \t\r\n]")
	       data)))
	(param  (string-match "@param" str))
	(access (string-match "@access" str))
	return)
    (when (string-match "this[ \t]+\\(function\\|method\\)" str)
      (my-lint-layout-message
       (format "[phpdoc] unnecessary wording: %s" (match-string 0 str))
       line prefix))
    (unless access
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
	 "[phpdoc] incorrect order. Should be @access..@return"
	 line prefix))))

(defun my-php-layout-doc-examine-content-function
  (str line &optional prefix data)
  "Examine content: function. Expects narrow to docstring.
STR is docstring at LINE number. PREFIX is for messages.
DATA is the full function content."
  (save-excursion
    ;;  * @param  $var string
    (goto-char (point-min))
    (let (word)
      (while (re-search-forward
	      "[*][ \t]*@param[ \t]+\\([^ \t\r\n]+\\)" nil t)
	(setq word (match-string 1))
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
			       my-lint-layout-php-data-type-short-regexp word)))))
		 (match (and case-p
			     (match-string 1 word))))
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
	     prefix)))))))

(defun my-php-layout-doc-string-test-var (str line &optional prefix)
  "Examine dostring: variable."
  (unless (string-match "@access" str)
    (my-lint-layout-message
     "[phpdoc] @access token not found"
     line
     prefix))
  (unless (string-match "@var" str))
  (my-lint-layout-message
   "[phpdoc] @var token not found"
   line
   prefix))

(defun my-php-layout-doc-string-test-class (str line &optional prefix)
  "Examine dostring: class."
  (unless (string-match "@package" str)
    (my-lint-layout-message
     "[phpdoc] @package token not found"
     line
     prefix)))

(defun my-php-layout-doc-examine-content-other
  (str line type &optional prefix)
  "Examine docstring: "
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (unless (looking-at "^[ \t]+[*]")
      (my-lint-layout-message
       "[phpdoc] Not a valid documentation comment."
       (1+ line)
       prefix))
    ;;  Complete sentence ends to period.
    (unless (looking-at "^[ \t]+[*].*\\.")
      (my-lint-layout-message
       "[phpdoc] First line is not a complete sentence ending to period(.)"
       (1+ line)
       prefix))
    (when (and (not
		(looking-at
		 (concat ".*"
			 my-lint-layout-generic-doc-1st-line-ignore-regexp)))
	       (not (looking-at
		     "^[ \t]+[*][ \t]+[^ \t\r\n]+[ \t][^ \t\r\n]+")))
      ;; Search at least two words. Ignore toplevel comment
      (when (not (memq 'file type))
	(my-lint-layout-message
	 "[phpdoc] First line does not explain code that follows"
	 (1+ line)
	 prefix)))
    (my-lint-layout-with-case
      (unless (looking-at "^[ \t]+[*][ \t]+[A-Z]")
	(my-lint-layout-message
	 "[phpdoc] decription does not start with capital letter."
	 (1+ line)
	 prefix)))
    (unless (or (memq 'include type)
		(memq 'class type))
      (forward-line 1)
      (my-lint-layout-with-case
	(unless (looking-at "^[ \t]*[*][ \t]*$")
	  (my-lint-layout-message
	   "[phpdoc] no empty line after first line short description."
	   (+ 2 line)
	   prefix)))
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*[*][ \t]@" nil t)
	(forward-line -1)
	(unless (looking-at "^[ \t]*[*][ \t]*$")
	  (my-lint-layout-message
	   "[phpdoc] no empty line before starting @-token"
	   (+ line (my-lint-layout-current-line-number))
	   prefix))))))

(defun my-php-layout-doc-examine-typeof (str)
  "Examine what type of docstring."
  (let (type)
    (if (my-lint-layout-type-variable-string-p str)
	(push 'var type))
    (if (my-lint-layout-type-class-string-p str)
	(push 'class type))
    (if (my-lint-layout-type-function-string-p str)
	(push 'function type))
    (if (my-lint-layout-type-include-string-p str)
	(push 'include type))
    type))

(defun my-php-layout-function-region-at-point ()
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

(defun my-php-layout-function-string-at-point ()
  "Return function string if any at point."
  (multiple-value-bind (beg end)
      (my-php-layout-function-region-at-point)
    (when beg
      (buffer-substring beg end))))

(defun my-php-layout-doc-examine-main (beg end type line &optional prefix)
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
	  (setq data (my-php-layout-function-string-at-point)))
	(narrow-to-region beg end)
	(let ((str (buffer-string)))
	  (cond
	   ((memq 'var type)
	    (my-php-layout-doc-string-test-var str line prefix))
	   ((memq 'class type)
	    (my-php-layout-doc-string-test-class str line prefix))
	   ((memq 'function type)
	    (my-php-layout-doc-string-test-function str line prefix data)
	    (my-php-layout-doc-examine-content-function str line prefix data)))
	  (my-php-layout-doc-examine-content-other str line type prefix))))))

(defun my-php-layout-check-doc-main (&optional prefix)
  "Check /** ... */"
  (let (line
	point
	next-line-valid-p
	str
	beg
	end
	type)
    (while (my-lint-layout-search-doc-beginning)
      (when (save-excursion
	      (setq point (point))
	      ;; Peek previous
	      (forward-line -1)
	      (my-lint-layout-current-line-string)
	      (unless (looking-at "^[ \t]*[{<]\\|^[ \t\r]*$")
		;; private $var;
		;; /**
		;;  * Documentation
		;;  */
		(my-lint-layout-message
		 "[newline] no empty line before documentation block"
		 (1+ (my-lint-layout-current-line-number))
		 prefix))
	      (forward-line 2)
	      (setq next-line-valid-p (my-lint-layout-looking-at-doc-p))
	      (setq end (my-lint-layout-search-doc-end))
	      (skip-chars-forward " \t\r\n")
	      (setq valid-p
		    (or (my-lint-layout-looking-at-doc-end-valid-p)
			;; File level comment
			(string-match "Copyright\\|License"
				      (buffer-substring point end))))
	      (setq type
		    (my-php-layout-doc-examine-typeof
		     (my-lint-layout-current-line-string)))
	      (if (and (not type)
		       valid-p)
		  (setq type '(file)))
	      end)
	(setq beg  (line-beginning-position)
	      str  (buffer-substring beg end)
	      line (my-lint-layout-current-line-number))
	(unless next-line-valid-p
	  (my-lint-layout-message
	   "[phpdoc] format layout error"
	   line prefix))
	(cond
	 ((my-lint-layout-doc-package-string-p str)) ;Skip
	 ((my-lint-layout-doc-var-string-p str)) ;Skip
	 ((not valid-p)
	  (my-lint-layout-message
	   "[phpdoc] possiby misplaced. Expecting class, func, var, require or include"
	   line prefix))
	 (t
	  (let ((top-level-p (my-lint-layout-doc-package-string-p str)))
	    (unless top-level-p
	      (my-php-layout-doc-examine-main
	       beg
	       end
	       type
	       line
	       prefix)))))))))

;;; ........................................................... &batch ...

(put 'my-php-layout-run-check 'lisp-indent-function 0)
(defmacro my-php-layout-run-check (&rest body)
  `(save-excursion
     ,@body))

(defun my-php-layout-check-all-1 (&optional prefix)
  "Run all checks from curent point. Does not display result buffer."
  (dolist (function
	   '(my-lint-layout-generic-class-count
	     my-lint-layout-generic-xml-tags-check-main
	     my-php-layout-check-xml-tags-lazy
	     my-php-layout-check-multiple-print
	     my-php-layout-check-statement-start
	     my-php-layout-check-comment-statements
	     my-php-layout-check-control-statements
	     my-php-layout-check-block-end-and-code
	     my-php-layout-check-line-up-assignment
	     my-php-layout-check-brace-extra-newline
	     my-php-layout-check-regexp-occur-main
	     my-php-layout-check-doc-missing
	     my-php-layout-check-doc-main
	     my-php-layout-check-multiline-print
	     my-php-layout-check-multiline-sql
	     my-php-layout-check-words
	     my-lint-layout-check-whitespace
	     my-lint-layout-check-eof-marker
	     ;; my-lint-layout-check-line-length
	     ))
    (my-php-layout-run-check
     (funcall function prefix))))

(defun my-php-layout-check-all-main ()
  "Run all checks from curent point."
  (interactive)
  (my-lint-layout-result-erase-buffer)
  (my-php-layout-check-all-1 (buffer-name))
  (my-lint-layout-result-sort-lines)
  (display-buffer my-lint-layout-buffer-name))

(defun my-php-layout-check-file-list (list &optional function)
  "Check LIST of files.
Run optional FUNCTION or `my-php-layout-check-all-1'."
  (let ((default-directory default-directory)
	(dir default-directory))
    (or function
	(setq function 'my-php-layout-check-all-1))
    (unless (listp list)
      (setq list (list list)))
    (when list
      (dolist (file list)
	(cd dir)
;;;        (message "2: %s %s %s" function file (file-exists-p file))
	(when (file-exists-p file)
	  (let (find-file-hooks)
	    (find-file file)
	    (goto-char (point-min))
	    (funcall function file))))
      (my-lint-layout-result-sort-lines))))

(defun my-php-layout-check-command-line-batch (&optional function)
  "Run FUNCTION (or list of) over files on command line."
  (let ((debug-on-error t)
	(files command-line-args-left)
	debug-ignored-errors
	(default-directory default-directory)
	(dir default-directory))
;;;    (message "function %s" function)
;;;    (message "files %s" files)
    (my-lint-layout-result-erase-buffer)
    (dolist (func (if (listp function)
		      function
		    (list function)))
      (save-excursion
	(goto-char (point-min))
	(my-php-layout-check-file-list files func)))
    (my-lint-layout-with-result-buffer
      ;; to stderr. Hm.
      (unless (eq (point-min) (point-max))
	(princ (buffer-string))))))


;;	(message
;;	 (replace-regexp-in-string "%" "%%" (buffer-string)))))))

;; End of file
