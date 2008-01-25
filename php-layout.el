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
 "\\<\\(abstract\\|public\\|protected\\|private\\|static\\)\\>"
 "Access modifiers.")

(defconst my-lint-layout-php-function-regexp
  (concat
   "\\(?:"
   my-lint-layout-generic-access-modifier-regexp
   "\\)?"
   "[ \t]*"
   "\\<function\\>")
 "Function regexp.")

(defconst my-lint-layout-php-doc-location-regexp
   (concat
    "^[ \t]*"
    "\\("
    my-lint-layout-generic-access-modifier-regexp
    "\\|function\\>\\|require\\|include"
    "\\)")
  "Location, where documentation should exist.")

(defconst my-lint-layout-generic-control-statement-start-regexp
  "\\<\\(if\\|while\\|for\\(?:each\\)\\|try\\)\\>"
  "Control statement keyword regexp.")

(defconst my-lint-layout-generic-control-statement-continue-regexp
   "\\<\\(else\\|catch\\)\\>"
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
  "\\<\\("
  "string"
  "\\|bool\\(?:ean\\)?"
  "\\|int\\(eger\\)?"
  "\\|mixed"
  "\\|array"
  "\\|object"
  "\\|none"
  "\\)\\>")
  "Valid datatypes.")

(defconst my-lint-layout-generic-doc-1st-line-ignore-regexp
  "constructor\\|destructor"
  "*Ignore first line wording check if regexp matches.")

(defconst my-lint-layout-generic-doc-line-regexp
  "^\\([ \t]*/[*][*]\\|[ \t]+[*]\\)"
  "Documentation comment line regexp.")

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

(defsubst my-lint-layout-prefix (prefix)
  "If PREFIX, format it line filename."
  (if prefix
      (format "%s:" prefix)
    ""))

(defsubst my-lint-layout-current-line-string ()
  "Current line."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun  my-lint-layout-current-line-number ()
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
   (concat
    "^[ \t]*"
    my-lint-layout-php-function-regexp)
   str))

(defsubst my-lint-layout-type-include-string-p (str)
  (string-match "^[ \t]*require\\|include" str))

(defsubst my-lint-layout-looking-at-doc-end-valid-p ()
  "Check of */ is followed by a function or variable definition."
  (and
   (looking-at
    (concat
     "[ \t]*\\<\\("
     "function\\|public\\|protected\\|private"
     "\\|static\\|var"
     "\\|require\\|include"
     "\\)\\>"))
   (match-string 1)))

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

(defun my-lint-layout-generic-mixed-eol-crlf (&optional prefix)
  "Check mixed CR/LF combination at end of line."
  (save-excursion
    (when (search-forward "\r" nil t)
      (my-lint-layout-message
       "[file-format] mixed CR and LF at the end of line (first occurrance)"
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-php-multiple-print (&optional prefix)
  "Check multiple print statements."
  (save-excursion
    (while (re-search-forward
            ;;  3 x threshold
            "^[ \t]*print.*\n[ \t]*print.*\n[ \t]*print" nil t)
      (my-lint-layout-message
       "Multiple print*() calls. Possible alternative: HERE syntax"
       (my-lint-layout-current-line-number)
       prefix))))

(defun my-lint-layout-generic-class-count (&optional prefix)
  "Count Classes and interfaces in one file"
  (let (count)
    (while (my-lint-layout-generic-class-forward)
      (unless (looking-at ".*PHPUnit")
        (if count
            (incf count)
          (setq count 1))))
      (when (and count
               (> count 1))
      (my-lint-layout-message
       (format "multiple classes or interfaces in same file: %d" count)
       ( my-lint-layout-current-line-number)
       prefix))))

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
             my-lint-layout-generic-control-statement-start-regexp)
            nil t)
      ;; Ignore comments, xml-tags.
      (unless (save-excursion
                (goto-char (match-beginning 1))
                (my-lint-layout-current-line-string)
                (looking-at "[*/#]\\|[<>][?]"))
        (my-lint-layout-message
         "[newlines] no empty line found between control statement and code above"
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
      (unless (string-match
               (concat
                my-lint-layout-generic-control-statement-regexp
                "\\|"
                my-lint-layout-generic-xml-tag-regexp)
               str)
        (setq line (my-lint-layout-current-line-number))
        (my-lint-layout-message
         "[newlines] no empty line found between '}' and next code line"
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
         "[comment] unknown syntax. Only // is recognized"
         (my-lint-layout-current-line-number)
         prefix))
      (unless (looking-at "[ \r\n]")
        (my-lint-layout-message
         "[comment] no space or newline after comment marker"
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
         ((my-lint-layout-type-include-string-p str))  ; Skip on "function files"
         ((my-lint-layout-type-statement-string-p str)
          (my-lint-layout-message
           "[phpdoc] variable not documented"
           line prefix)))))))

(defun my-php-layout-extra-newlines (&optional msg prefix)
  "Check extra newlines at point."
  (when (looking-at "\\(\\(?:[ \t]*\r?\n\\)+\\)")
    (let ((str (match-string 0))
          (line (my-lint-layout-current-line-number)))
      (my-lint-layout-message
       (format "[newlines] extra %d%s"
               (my-lint-layout-count-lines-in-string str)
               (or msg ""))
       line prefix))))

(defun my-php-layout-check-whitespace (&optional prefix)
  "Check Whitespace documentation."
  (let (line)
    (save-excursion
      (my-php-layout-extra-newlines " at beginning of file" prefix)
      (while (re-search-forward "[ \t]+$" nil t)
        (setq line (my-lint-layout-current-line-number))
        (my-lint-layout-message
         "trailing whitepace(s) at end of line"
         line prefix))
      (goto-char (point-max))
      (when (re-search-backward "[^ \t\r\n]\\(\n\\)?" nil t)
        (cond
         ((string= "\n" (match-string 1))
          ;; eob trailing newlines?
          (forward-line 1)
          (my-php-layout-extra-newlines " at end of file" prefix))
         (t
          (setq line (my-lint-layout-current-line-number))
          (my-lint-layout-message
           "[newlines] missing LF-character from the last line of file"
           line prefix)))))))

(defun my-php-layout-check-line-length (&optional prefix)
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


(defsubst my-php-layout-indent-level (str)
  "Count indent."
  (and (string-match "^[ ]*" str)
       (length (match-string 0 str))))

(defun my-php-layout-check-indent-string-message (str line &optional prefix)
  (let ((i (my-php-layout-indent-level str)))
    (when (and (> i 0)
               (not (zerop (mod i 4))))
      (my-lint-layout-message
       (format "indent level %d is not multiple of %d (space character count)"
               i
               my-lint-layout-generic-indent-step)
       line
       prefix))))

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

(defun my-php-layout-check-statement-start (&optional prefix)
  "Check lines beyond `my-lint-layout-generic-line-length-max'."
  (let* ((col my-lint-layout-generic-line-length-max)
         (re (concat "^"
                     "\\([ \t]*\\)"
                     "\\("
                     my-lint-layout-generic-control-statement-regexp
                     "\\)"))
         str
         indent
         brace-p
         statement-start-col
         statement-line
         brace-start-col
         brace-start-line
         brace-end-col
         brace-end-line)
    (while (re-search-forward re nil t)
      (setq indent (match-string 1)
            str    (match-string 2))
      (save-excursion
        (goto-char (match-beginning 2))
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
          (setq brace-p (not (looking-at ".*;")))))
      (my-php-layout-check-indent-string-message indent statement-line prefix)
      (when (string-match my-lint-layout-generic-control-statement-continue-regexp str)
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
        (setq brace-start-col (current-column)
              brace-start-line (my-lint-layout-current-line-number))
        (unless (eq statement-start-col brace-start-col)
          (my-lint-layout-message
           (format "brace { not directly under keyword '%s', expect col %d"
                   str
                   statement-start-col)
           (my-lint-layout-current-line-number)
           prefix)))))))

;;; ....................................................... &changelog ...

(defconst my-layout-changelog-item-regexp
  "^[ \t][*]\\( *\\)\\([^ :()\t\r\n]+\\)")

(defun my-layout-changelog-check-main (&optional prefix)
  "Check ChangeLog syntax."
  (let (indent
        file)
    (my-lint-layout-generic-mixed-eol-crlf prefix)
    (while (re-search-forward my-layout-changelog-item-regexp nil t)
      (setq indent (match-string 1)
            file   (match-string 2))
      (unless (looking-at ":")
        (my-lint-layout-message
         "[changelog] filename does not end to colon ':'"
         (my-lint-layout-current-line-number)
         prefix))
      (when (and (string-match " " indent)
                 (not (string= " " indent)))
        (my-lint-layout-message
         "[changelog] no exactly one space after '*'"
         (my-lint-layout-current-line-number)
         prefix))
      (when (and (not (string-match " " indent))
                 (not (string= " " indent)))
        (my-lint-layout-message
         "[changelog] '*' does not have following space"
         (my-lint-layout-current-line-number)
         prefix))
      (when (and (not (string-match " " indent))
                 (not (string= " " indent)))
        (my-lint-layout-message
         "[changelog] '*' does not have following space"
         (my-lint-layout-current-line-number)
         prefix))
      (forward-line 1))))

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

(defconst my-php-layout-brace-and-code-regexp
  "[}][ \t]*\r?\n[ \t]*\\([^{} \t\r\n]+\\)"
  "Match brace end } followed by immediate code.")

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
            "[newlines] extra %d above. See previous brace '{'")
           (end
            "[newlines] extra %d before ending brace '}'")
           (function-before
            "[newlines] extra %d before function definition")
           (empty
            "[newlines] empty brace block found")))
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
      '(
        "AVG"
        "COUNT"
        "MAX"
        "MIN"
        "SUM"
        ) t) "\\b"))
  "SQL reserved keywords.")

(defconst my-lint-layout-sql-keywords-column-mysql
  (eval-when-compile
    (concat
     "\\b"
     (regexp-opt
      '(
        "AUTO_INCREMENT"
        "UNSIGNED"
        "ZEROFILL"
        ;; CREATE TABLE xxx (...) ENGINE = InnoDB;
        "ENGINE"
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
        "DEC"
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

(defconst my-lint-layout-sql-keywords-all
  (concat
   "\\("
   my-lint-layout-sql-keywords-reserved
   "\\|"
   my-lint-layout-sql-keywords-functions
   "\\|"
   my-lint-layout-sql-keywords-sql92-data-types
   "\\)")
  "SQL reserved keywords.")

(defun my-lint-layout-sql-check-keywords (&optional prefix)
  "Check SQL syntax."
  (require 'sql)
  (let ((sql-re my-lint-layout-sql-keywords-all)
        (type-re my-lint-layout-sql-keywords-sql92-data-types)
        (mysql-re my-lint-layout-sql-keywords-column-mysql)
        (step my-lint-layout-generic-indent-step)
        str
        tmp
        datatype
        word)
      (while (re-search-forward "^\\([ \t]*[^-\r\n].*\\)" nil t)
        (setq str (match-string 1))
        (when (string-match "#\\|/[*]" str)
          (my-lint-layout-message
           (format "[sql] non-standard comment syntax: %s" str)
           (my-lint-layout-current-line-number)
           prefix))
        (when (string-match mysql-re str)
          (my-lint-layout-message
           (format "[sql] non-standard keyword: %s" (match-string 0 str))
           (my-lint-layout-current-line-number)
           prefix))
        (setq word nil)
        (cond
         ((string-match "^[ \t]*\\(.+[^ \t]+\\)[ \t]+NOT[ \t]+NULL" str)
          (setq tmp (match-string 1 str))
          ;; FIXME "FLOAT(1, 2)"
          (when (string-match "\\(\\([a-z]+\\)\\(([ \t]*[,0-9 \t]+)\\)?\\)$" tmp)
            (setq word (match-string 2 tmp))))
         ((string-match "NULL" str)
          (when (string-match "NULL" str)  ;; FIXME: NULL itself is not SQL92
            nil)))
        (when word
          (unless (string-match type-re word)
            (my-lint-layout-message
             (format "[sql] unknown datatype: %s" word)
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
       (format "[SQL] Portability problem near table name: %s" table)
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
      (when (string-match "[(]" line)
        (my-lint-layout-message
         "[sql] misplaced starting paren '(' (possibly not lined-up)"
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
             (not (looking-at ".*[a-z]:[ \t]")))
    (my-lint-layout-message
     "[css] no space after attribute colon ':'"
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

(defun my-lint-layout-css-check (&optional prefix)
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

(defun my-php-layout-doc-string-test-function (str line &optional prefix)
  (let (access param return)
    (when (string-match "this[ \t]+\\(function\\|method\\)" str)
      (my-lint-layout-message
       (format "[phpdoc] unnecessary wording: %s" (match-string 0 str))
       line prefix))
    (unless (setq access (string-match "@access" str))
      (my-lint-layout-message
       "[phpdoc] @access token not found"
       line prefix))
    (unless (setq param (string-match "@param" str))
      (my-lint-layout-message
       "[phpdoc] @param token not found"
       line prefix))
    (unless (setq return (string-match "@return" str))
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

(defun my-php-layout-doc-examine-content-function (str line &optional prefix)
  "Examine content: function. Expects narrow to docstring."
  (save-excursion
    ;;  * @param  $var string
    (goto-char (point-min))
    (let (word)
      (while (re-search-forward
              "[*][ \t]*@param[ \t]+\\([^ \t\r\n]+\\)" nil t)
        (setq word (match-string 1))
        (unless (let (case-fold-search)
                  (string-match my-lint-layout-php-data-type-regexp
                                word))
          (let* ((case (string-match
                        my-lint-layout-php-data-type-regexp word))
                 (match (and case
                             (match-string 1 word))))
            (my-lint-layout-message
             (concat
              "@param datatype should be a valid type or "
              "'mixed'"
              (if case
                  (format " (check spelling of '%s')" match)
                "")
              ". See PHP manual 'Types'.")
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
   "[phpdoc] @var not found"
   line
   prefix))

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
       "[phpdoc] 1st line is not a complete sentence ending to period(.)"
       (1+ line)
       prefix))
    (when (and (not (looking-at
                     (concat ".*"
                             my-lint-layout-generic-doc-1st-line-ignore-regexp)))
               (not (looking-at
                     "^[ \t]+[*][ \t]+[^ \t\r\n]+[ \t][^ \t\r\n]+")))
      ;; Search at least two words
      (my-lint-layout-message
       "[phpdoc] 1st line does not explain code that follows"
       (1+ line)
       prefix))
    (let (case-fold-search)
      (unless (looking-at "^[ \t]+[*][ \t]+[A-Z]")
        (my-lint-layout-message
         "[phpdoc] decription does not start with capital letter."
         (1+ line)
         prefix)))
    (unless (memq 'include type)
      (forward-line 1)
      (let (case-fold-search)
        (unless (looking-at "^[ \t]*[*][ \t]*$")
          (my-lint-layout-message
           "[phpdoc] no empty line after 1st line short description."
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
    (if (my-lint-layout-type-function-string-p str)
        (push 'function type))
    (if (my-lint-layout-type-include-string-p str)
        (push 'include type))
    type))

(defun my-php-layout-doc-examine-main (beg end type line &optional prefix)
  "Examine docstring
  ;;    /**
  ;;     * Short description
  ;;     *
  ;;     * Full description .....
  ;;     */"
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      (let ((str (buffer-string)))
        (cond
         ((memq 'var type)
          (my-php-layout-doc-string-test-var str line prefix))
         ((memq 'function type)
          (my-php-layout-doc-string-test-function str line prefix)
          (my-php-layout-doc-examine-content-function str line prefix)))
        (my-php-layout-doc-examine-content-other str line type prefix)))))

(defun my-php-layout-check-doc-main (&optional prefix)
  "Check /** ... */"
  (let (line
        nex-line-valid-p
        str
        beg
        end
        type)
    (while (my-lint-layout-search-doc-beginning)
      (when (save-excursion
              ;; Peek previous
              (forward-line -1)
              (my-lint-layout-current-line-string)
              (unless (looking-at "^[ \t]*[{<]\\|^[ \t\r]*$")
                ;; private $var;
                ;; /**
                ;;  * Documentation
                ;;  */
                (my-lint-layout-message
                 "[newlines] no empty line before documentation block"
                 (1+ (my-lint-layout-current-line-number))
                 prefix))
              (forward-line 2)
              (setq nex-line-valid-p (my-lint-layout-looking-at-doc-p))
              (setq end (my-lint-layout-search-doc-end))
              (skip-chars-forward " \t\r\n")
              (setq valid-p (my-lint-layout-looking-at-doc-end-valid-p))
              (setq type
                    (my-php-layout-doc-examine-typeof
                     (my-lint-layout-current-line-string)))
              end)
        (setq beg  (line-beginning-position)
              str  (buffer-substring beg end)
              line (my-lint-layout-current-line-number))
        (unless nex-line-valid-p
          (my-lint-layout-message
           "[phpdoc] format layout error"
           line prefix))
        (cond
         ((my-lint-layout-doc-package-string-p str)) ;Skip
         ((my-lint-layout-doc-var-string-p str)) ;Skip
         ((not valid-p)
          (my-lint-layout-message
           "[phpdoc] not located at function, variable or require"
           line prefix))
         (t
          (setq top-level-p (my-lint-layout-doc-package-string-p str))
          (unless top-level-p
            (my-php-layout-doc-examine-main
             beg
             end
             type
             line
             prefix))))))))

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
             my-lint-layout-php-multiple-print
             my-php-layout-check-statement-start
             my-php-layout-check-comment-statements
             my-php-layout-check-control-statements
             my-php-layout-check-block-end-and-code
             my-php-layout-check-line-up-assignment
             my-php-layout-check-brace-extra-newline
             my-php-layout-check-doc-missing
             my-php-layout-check-doc-main
             my-php-layout-check-whitespace
             my-php-layout-check-line-length))
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
  (let ((debug-on-error t)
        (files command-line-args-left)
        debug-ignored-errors
        (default-directory default-directory)
        (dir default-directory))
;;;    (message "function %s" function)
;;;    (message "files %s" files)
    (my-lint-layout-result-erase-buffer)
    (my-php-layout-check-file-list files function)
    (my-lint-layout-with-result-buffer
      ;; to stderr. Hm.
      (unless (eq (point-min) (point-max))
        (message (buffer-string))))))

;; End of file
