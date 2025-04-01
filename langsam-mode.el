;;; langsam-mode.el --- Major mode for Langsam code -*- lexical-binding: t; -*-

;;; Code:

(defvar langsam-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars
    (modify-syntax-entry ?' "'" table)
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?, "'" table)
    (modify-syntax-entry ?@ "'" table)

    ;; Others
    (modify-syntax-entry ?\; "<" table) ; comment start
    (modify-syntax-entry ?\n ">" table) ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for Langsam mode.")

(defun langsam-mode-font-lock-keywords ()
  `((,(rx (any "A-Z") (0+ (syntax word)))
     (0 font-lock-type-face))
    (,(rx "(" (group (or
                      "fn*"
                      "fn"
                      "macro"
                      "def"
                      "defn"
                      "defmacro"
                      "progn"
                      "let"
                      "let1"
                      "if"
                      "when"
                      "and" "or" "not"
                      "throw"
                      "type")) word-end)
     (1 font-lock-keyword-face))
    (,(rx symbol-start (or "true" "false" "nil") symbol-end)
     (0 font-lock-constant-face))
    (,(rx symbol-start ":" (0+ (or (syntax word) (syntax symbol))))
     (0 font-lock-constant-face))
    (,(rx "(" (group (1+ (or (syntax word) (syntax symbol)))) word-end)
     (1 font-lock-function-name-face))))

(define-derived-mode langsam-mode
  lisp-data-mode "Langsam"
  (setq font-lock-defaults '(langsam-mode-font-lock-keywords))
  (setq-local lisp-indent-offset 2))

(add-to-list 'auto-mode-alist '("\\.l\\'" . langsam-mode))

(provide 'langsam-mode)
