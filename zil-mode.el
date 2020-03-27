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

;;; TODO

;;  - indentation rules
;;  - clean-up list of keywords
;;  - top-level "..." as comments
;;  - recognize "AUX", "OPTIONAL" as built-ins and not strings
;;  - recognize following as multi-line comment
;;        ; <ROUTINE ...
;;                   ...>

;;; Code:

(defvar zil-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\; "<"  st)
    (modify-syntax-entry ?\n ">"  st)
    (modify-syntax-entry ?\- "w" st)
    (modify-syntax-entry ?\? "w" st)
    (modify-syntax-entry ?\< "(" st)
    (modify-syntax-entry ?\> ")" st)
    (modify-syntax-entry ?\" "\"" st)
    st))

(defvar zil-mode-abbrev-table nil)
(define-abbrev-table 'zil-mode-abbrev-table ())

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

;;; (makunbound 'zil-font-lock-keywords)
(defvar zil-font-lock-keywords
  (eval-when-compile
    (list
     (cons
      (concat
       "<" (regexp-opt
	    '("SETG" "COND" "AND" "OR" "NOT" "ADD" "SUB" "MUL" "DIV" "MOD"
	      "RANDOM" "EQUAL?" "ZERO?"  "LESS?"  "GRTR?" "FSET?" "MOVE"
	      "REMOVE" "LOC" "FIRST?"  "NEXT?" "FSET" "FCLEAR" "GETP" "PUTP"
	      "GET" "PUT" "INTBL?"  "COPYT" "READ" "INPUT" "MOUSE-INFO"
	      "MOUSE-LIMIT" "MENU" "PRINT" "PRINTD" "PRINTN" "BUFOUT" "CRLF"
	      "HLIGHT" "COLOR" "DIROUT" "DIRIN" "CURSET" "CURGET" "SCREEN"
	      "CLEAR" "WINPOS" "WINSIZE" "WINATTR" "SPLIT" "MARGIN" "SCROLL"
	      "DISPLAY" "PICINF" "SOUND" "CALL" "RTRUE" "RFALSE" "QUIT"
	      "RESTART" "VERIFY" "SAVE" "RESTORE" "ISAVE" "IRESTORE" "BTST"
	      "BAND" "BOR" "BCOM" "PUTB" "SET" "DEC" "PRINTC" "BCOM" "SHIFT"
	      "ASHIFT" "NEXTP" "GETB" "PUTB" "GETPT" "PTSIZE" "VALUE" "SET"
	      "ASSIGNED?" "INC" "DEC" "IGRTR?"  "DLESS?" "PUSH" "XPUSH" "POP"
	      "FSTACK" "LEX" "ZWSTR" "PRINTC" "PRINTB" "PRINTI" "PRINTR"
	      "PRINTT" "PRINTF" "FONT" "ERASE" "WINGET" "WINPUT" "DCLEAR"
	      "CALL1" "CALL2" "XCALL" "ICALL1" "ICALL2" "ICALL" "IXCALL"
	      "CATCH" "THROW" "JUMP" "RSTACK" "NOOP" "ORIGINAL?" "+" "-" "*"
	      "/") t)
       "\\>") 1)
     '("<\\(ROUTINE\\|DEFMAC\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-function-name-face))
     '("<\\(CONSTANT\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-constant-face))
     '("<\\(GLOBAL\\|DEFINE\\)\\s-*\\(\\sw+\\)"
       (1 font-lock-keyword-face) (2 font-lock-variable-name-face))
     ;; probably need to override syntax-propertize-function to make these work..
     '("\\(\"AUX\"\\|\"OPTIONAL\"\\)"
       (1 font-lock-builtin-face))))
  "Expressions to highlight in ZIL modes.")

(define-derived-mode zil-mode prog-mode "ZIL"
  "Major mode for editing ZIL code.

Entering this mode runs the hooks `zil-mode-hook'.
..."
  :syntax-table zil-mode-syntax-table
  :abbrev-table zil-mode-abbrev-table
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-generic-expression zil-imenu-generic-expression)
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
