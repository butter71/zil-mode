;;; zil-mode.el --- ZIL (Zork Implementation Language) editing mode    -*- lexical-binding: t -*-

;; Copyright (C) 2020

;; Author:
;; Maintainer:
;; Package-Requires:
;; Keywords: zil
;; Version: 0.0.0

;; This file is not part of Emacs

;;; Commentary:

;; The major mode for editing ZIL (Zork Implementation Language) code.
;; Keyword list borrowed from:
;;   https://bitbucket.org/zilf/vscode-zil-language/src/default/data/builtins/ZILF%200.8.json

;;; TODO

;;  - indentation rules

;;; Code:

(defvar zil-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-;") 'zil-comment-dwim)
    map)
  "Keymap for zil-mode.")

(defvar zil-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" st)
    (modify-syntax-entry '(?a . ?z) "w" st)
    (modify-syntax-entry '(?A . ?Z) "w" st)
    (modify-syntax-entry ?- "w" st)
    (modify-syntax-entry ?\? "w" st)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" st)
    (modify-syntax-entry ?\) ")(" st)
    (modify-syntax-entry ?\< "(>" st)
    (modify-syntax-entry ?\> ")<" st)

    ;; Prefixes
    (modify-syntax-entry ?\, "'" st)
    (modify-syntax-entry ?\. "'" st)

    ;; Misc
    (modify-syntax-entry ?\\ "\\" st)
    (modify-syntax-entry ?\" "\"" st)
    st))

(defun zil--top-level-commentize ()
  "Put `syntax-table' property correctly on top-level ?\\\" comments."
  (let ((ppss (save-excursion (backward-char) (syntax-ppss))))
    (cond ((nth 5 ppss)			; it's quoted
	   nil)
	  ;; manually check if quoted.  not sure if needed because
	  ;;   we're in a comment or outside a string
	  ((eq ?\\ (char-before (1- (point))))
	   nil)
	  ((> (car ppss) 0)		; not top-level
	   nil)
	  ((nth 3 ppss)			; inside a string
	   nil)
	  ((nth 4 ppss)			; closing quote
	   (put-text-property (1- (point)) (point)
			      'syntax-table (string-to-syntax ">")))
	  ((eq ?\; (char-before (1- (point)))) ; opening quote, but inside ?\; comment
	   nil)
	  (t				; opening quote
	   (put-text-property (1- (point)) (point)
			      'syntax-table (string-to-syntax "<"))))))

(defconst zil-syntax-propertize-function
  (syntax-propertize-rules
   ;; "AUX", "OPTIONAL", ... are used for keyword args
   ;;   TODO: check it's within an arglist and not just a random string.
   ((rx (group "\"") (or "AUX" "OPTIONAL" "OPT" "TUPLE" "ARGS") (group "\""))
    (1 "_") (2 "_"))
   ("\""
    (0 (ignore (zil--top-level-commentize)))))
   "Syntax property rules for ZIL mode special cases.")

(define-abbrev-table 'zil-mode-abbrev-table ()
  "Abbrev table for ZIL mode.")

(defvar zil-imenu-generic-expression
  '((nil
     "^<\\(ROUTINE\\|DEFINE\\)\\s-+\\(\\sw+\\)" 2)
    ("Macros"
     "^<DEFMAC\\s-+\\(\\sw+\\)" 1)
    ("Constants"
     "^<CONSTANT\\s-+\\(\\sw+\\)" 1)
    ("Globals"
     "^<GLOBAL\\s-+\\(\\sw+\\)" 1)
    ("Rooms"
     "^<ROOM\\s-+\\(\\sw+\\)" 1)
    ("Objects"
     "^<OBJECT\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for ZIL mode.  See `imenu-generic-expression'.")

(defun zil--search-commented-sexps-internal (limit)
  "Search for a ?\\; comment forward stopping at LIMIT."
  (when (search-forward-regexp "\\(;\\)" limit t)
    (let* ((md (match-data))
           (start (match-beginning 1))
           (state (syntax-ppss start)))
      ;; inside string or comment?
      (if (or (nth 3 state)
              (nth 4 state))
          (zil--search-commented-sexps-internal limit)
	(goto-char (1+ start))
	(forward-sexp 1)
	(setf (elt md 3) (point))
        (set-match-data md)
        t))))

(defun zil--search-commented-sexps (limit)
  "Find commented sexps and set the match data.
Search from point up to LIMIT.  The region that should be
considered a comment is between `(match-beginning 1)'
and `(match-end 1)'."
  (let ((result 'retry))
    (while (and (eq result 'retry) (<= (point) limit))
      (condition-case nil
          (setq result (zil--search-commented-sexps-internal limit))
        (end-of-file (setq result nil))
        (scan-error  (setq result 'retry))))
    result))

(defvar zil-font-lock-keywords
  (eval-when-compile
    `((,(concat
	 "<" (regexp-opt
	      '("*" "+" "-" "/" "0?" "1?" "==?" "=?" "ADD" "ADD-TELL-TOKENS"
		"ADD-WORD" "ADJ-SYNONYM" "AGAIN" "ALLTYPES" "AND" "AND?" "ANDB"
		"APPLICABLE?" "APPLY" "APPLYTYPE" "ASCII" "ASH" "ASHIFT"
		"ASK-FOR-PICTURE-FILE?" "ASSIGNED?" "ASSOCIATIONS" "ATOM" "AVALUE"
		"BACK" "BAND" "BCOM" "BEGIN-SEGMENT" "BIND" "BIT-SYNONYM" "BLOAT"
		"BLOCK" "BOR" "BOUND?" "BTST" "BUFOUT" "BUZZ" "BYTE" "CALL"
		"CATCH" "CHECK-VERSION?" "CHECKPOINT" "CHECKU" "CHRSET" "CHTYPE"
		"CLEAR" "CLOSE" "COLOR" "COMPILATION-FLAG"
		"COMPILATION-FLAG-DEFAULT" "COMPILATION-FLAG-VALUE" "COMPILING?"
		"COND" "CONS" "CONSTANT" "COPYT" "CRLF" "CURGET" "CURSET" "DCLEAR"
		"DEC" "DECL-CHECK" "DECL?" "DEFAULT-DEFINITION" "DEFAULTS-DEFINED"
		"DEFINE" "DEFINE-GLOBALS" "DEFINE-SEGMENT" "DEFINE20"
		"DEFINITIONS" "DEFMAC" "DEFSTRUCT" "DELAY-DEFINITION"
		"DESC-BUILTINS!-YOMIN" "DIR-SYNONYM" "DIRECTIONS" "DIRIN" "DIROUT"
		"DISPLAY" "DIV" "DLESS?" "DO" "EMPTY?" "END-DEFINITIONS"
		"END-SEGMENT" "ENDBLOCK" "ENDLOAD" "ENDPACKAGE" "ENDSECTION"
		"ENTRY" "EQUAL?" "EQVB" "ERASE" "ERROR" "EVAL" "EVAL-IN-SEGMENT"
		"EVALTYPE" "EXPAND" "F?" "FCLEAR" "FILE-FLAGS" "FILE-LENGTH"
		"FIRST?" "FLOAD" "FONT" "FORM" "FREQUENT-WORDS?" "FSET" "FSET?"
		"FSTACK" "FUNCTION" "FUNNY-GLOBALS?" "G=?" "G?" "GASSIGNED?"
		"GBOUND?" "GC" "GC-MON" "GDECL" "GET" "GET-DECL" "GETB" "GETP"
		"GETPROP" "GETPT" "GLOBAL" "GROW" "GRTR?" "GUNASSIGN" "GVAL"
		"HLIGHT" "ID" "IFFLAG" "IGRTR?" "ILIST" "IMAGE" "IN?" "INC"
		"INCLUDE" "INCLUDE-WHEN" "INDENT-TO" "INDEX" "INDICATOR" "INPUT"
		"INSERT" "INSERT-FILE" "INTBL?" "IRESTORE" "ISAVE" "ISTRING"
		"ITABLE" "ITEM" "IVECTOR" "L=?" "L?" "LANGUAGE" "LEGAL?" "LENGTH"
		"LENGTH?" "LESS?" "LEX" "LINK" "LIST" "LOC" "LONG-WORDS?" "LOOKUP"
		"LOWCORE" "LOWCORE-TABLE" "LPARSE" "LSH" "LTABLE" "LVAL" "M-HPOS"
		"MAKE-GVAL" "MAP-CONTENTS" "MAP-DIRECTIONS" "MAPF" "MAPLEAVE"
		"MAPR" "MAPRET" "MAPSTOP" "MARGIN" "MAX" "MEMBER" "MEMQ" "MENU"
		"MIN" "MOBLIST" "MOD" "MOUSE-INFO" "MOUSE-LIMIT" "MOVE" "MSETG"
		"MUL" "N==?" "N=?" "NEVER-ZAP-TO-SOURCE-DIRECTORY?" "NEW-ADD-WORD"
		"NEWTYPE" "NEXT" "NEXT?" "NEXTP" "NOT" "NTH" "OBLIST?"
		"OFFSET" "OPEN" "OR" "OR?" "ORB" "ORDER-FLAGS?" "ORDER-OBJECTS?"
		"ORDER-TREE?" "ORIGINAL?" "PACKAGE" "PARSE" "PICFILE" "PICINF"
		"PICSET" "PLTABLE" "PNAME" "POP" "PREP-SYNONYM" "PRIMTYPE" "PRIN1"
		"PRINC" "PRINT" "PRINT-MANY" "PRINTB" "PRINTC" "PRINTD" "PRINTF"
		"PRINTI" "PRINTN" "PRINTR" "PRINTT" "PRINTTYPE" "PRINTU" "PROG"
		"PROPDEF" "PTABLE" "PTSIZE" "PUSH" "PUT" "PUT-DECL"
		"PUT-PURE-HERE" "PUTB" "PUTP" "PUTPROP" "PUTREST" "QUIT" "QUOTE"
		"RANDOM" "READ" "READSTRING" "REMOVE" "RENTRY" "REPEAT"
		"REPLACE-DEFINITION" "REST" "RESTART" "RESTORE" "RETURN" "RFALSE"
		"RFATAL" "ROOT" "ROUTINE" "ROUTINE-FLAGS" "RSTACK" "RTRUE"
		"SAVE" "SCREEN" "SCROLL" "SET" "SET-DEFSTRUCT-FILE-DEFAULTS"
		"SETG" "SETG20" "SHIFT" "SNAME" "SORT" "SOUND" "SPLIT" "SPNAME"
		"STACK" "STRING" "STRUCTURED?" "SUB" "SUBSTRUC" "SYNONYM" "SYNTAX"
		"T?" "TABLE" "TELL" "TELL-TOKENS" "THROW" "TIME" "TOP"
		"TYPE" "TYPE?" "TYPEPRIM" "UNASSIGN" "UNPARSE" "USE" "USE-WHEN"
		"USL" "VALID-TYPE?" "VALUE" "VECTOR" "VERB-SYNONYM" "VERIFY"
		"VERSION" "VERSION?" "VOC" "WARN-AS-ERROR?" "WINATTR" "WINGET"
		"WINPOS" "WINPUT" "WINSIZE" "XFLOAD" "XORB" "XPUSH" "ZAPPLY"
		"ZBACK" "ZBUFOUT" "ZCRLF" "ZERO?" "ZGET" "ZIP-OPTIONS" "ZPACKAGE"
		"ZPRINT" "ZPRINTB" "ZPUT" "ZRANDOM" "ZREAD" "ZREMOVE" "ZREST"
		"ZRESTORE" "ZSAVE" "ZSECTION" "ZSTART" "ZSTR-OFF" "ZSTR-ON"
		"ZWSTR" "ZZPACKAGE" "ZZSECTION") t)
	 "\\>") (1 font-lock-keyword-face))
      ("<\\(ROUTINE\\|DEFMAC\\|DEFINE\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))
      ("<\\(CONSTANT\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-constant-face))
      ("<\\(GLOBAL\\|ROOM\\|OBJECT\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
      (,(rx "\"" (or "AUX" "OPTIONAL" "OPT" "TUPLE" "ARGS") "\"")
       (0 font-lock-builtin-face))
      (zil--search-commented-sexps (1 font-lock-comment-face t))))
  "Expressions to highlight in ZIL mode.")

(defun zil--find-comment ()
  "Return position of ?\\; if point is located within a comment.
Otherwise return nil."
  (if (looking-at ";")
      (point)
    (save-excursion
      (skip-syntax-backward "w_'\"")
      (condition-case nil
	  (progn
	    (while (not (eq ?\; (char-before (point))))
	      (up-list -1 t))
	    (1- (point)))
	(beginning-of-file nil)
	(scan-error        nil)))))

(defun zil-comment-dwim (n)
  "Toggle comment status of sexp at point.  With a prefix, go up N
sexps and add comment there."
  (interactive "p")

  (save-excursion
   (let ((cpos (zil--find-comment)))
     (cond
      ((integerp cpos)
       (goto-char cpos)
       (delete-char 1))
      (t
       (save-excursion
	 (skip-syntax-backward "w_'\"")
	 (up-list (- (1- n)) t)
	 (insert ";")))))))

(define-derived-mode zil-mode prog-mode "ZIL"
  "Major mode for editing ZIL code.

Entering this mode runs the hook `zil-mode-hook'.

\\{zil-mode-map}"
  :syntax-table zil-mode-syntax-table
  :abbrev-table zil-mode-abbrev-table
  (setq-local comment-use-syntax nil)
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression zil-imenu-generic-expression)
  (setq-local font-lock-multiline t)
  (setq-local syntax-propertize-function zil-syntax-propertize-function)
  (setq font-lock-defaults '(zil-font-lock-keywords)))

(defgroup zil nil
  "Editing ZIL code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'languages)

(defcustom zil-mode-hook nil
  "Normal hook run when entering `zil-mode'.
See `run-hooks'."
  :type 'hook
  :group 'zil)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.zil\\'" . zil-mode))

(provide 'zil-mode)
