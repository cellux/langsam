;;; langsam-mode.el --- Major mode for Langsam code -*- lexical-binding: t; -*-
;;
;; Author: Balazs Ruzsa <ruzsa.balazs@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, lisp, langsam
;; URL: https://github.com/cellux/langsam
;;
;;; Commentary:
;;
;; Put this file on your load-path, then:
;;   (add-to-list 'auto-mode-alist '("\\.l\\'" . langsam-mode))
;;
;;; Code:

(require 'imenu)
(require 'rx)
(require 'comint)

(defgroup langsam nil
  "Major mode for the Langsam language."
  :group 'languages)

;;;; User options --------------------------------------------------------------

(defcustom langsam-repl-executable "langsam"
  "Executable used to start the Langsam REPL."
  :type 'string :group 'langsam)

;;;; Syntax table --------------------------------------------------------------

(defvar langsam-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; ; line comments
    (modify-syntax-entry ?\; "<" st)
    (modify-syntax-entry ?\n ">" st)
    ;; Symbol constituents common in Lisps
    (modify-syntax-entry ?! "_" st)
    (modify-syntax-entry ?$ "_" st)
    (modify-syntax-entry ?% "_" st)
    (modify-syntax-entry ?& "_" st)
    (modify-syntax-entry ?* "_" st)
    (modify-syntax-entry ?+ "_" st)
    (modify-syntax-entry ?. "_" st)
    (modify-syntax-entry ?/ "_" st)
    (modify-syntax-entry ?: "_" st)
    (modify-syntax-entry ?< "_" st)
    (modify-syntax-entry ?= "_" st)
    (modify-syntax-entry ?> "_" st)
    (modify-syntax-entry ?? "_" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?~ "_" st)
    ;; Expression prefixes
    (modify-syntax-entry ?' "'" st)
    (modify-syntax-entry ?` "'" st)
    (modify-syntax-entry ?, "'" st)
    (modify-syntax-entry ?@ "'" st)
    (modify-syntax-entry ?# "'" st)
    (modify-syntax-entry ?^ "'" st)
    (modify-syntax-entry ?| "'" st)
    st))

;;;; Font-lock -----------------------------------------------------------------

(rx-let ((sym (+ (any "a-z" "A-Z" "0-9" ?- "!$%&*+./:<=>?_~")))
         (def-like (or "def"))
         (defn-like (or "defn" "defmacro" "defmulti"))
         (fn-like (or "fn" "macro")))
  (defconst langsam--number-rx (rx symbol-start
                                   (? (any "+-"))
                                   (or (+ digit)
                                       (seq (+ digit) "." (+ digit))
                                       (seq "0x" (+ (any xdigit)))
                                       (seq "0o" (+ (any "0-7")))
                                       (seq "0b" (+ (any "0-1"))))
                                   symbol-end))
  (defconst langsam--keyword-rx (rx ?: sym))
  (defconst langsam--opword-rx (rx ?% sym))
  (defconst langsam--nil-rx (rx symbol-start "nil" symbol-end))
  (defconst langsam--boolean-rx (rx symbol-start (or "true" "false") symbol-end))
  (defconst langsam--typename-rx (rx symbol-start "A-Z" sym symbol-end))
  (defconst langsam--def*-rx (rx "(" (* space) (group def-like) (+ space) (group sym) symbol-end))
  (defconst langsam--def-rx (rx symbol-start def-like symbol-end))
  (defconst langsam--defn*-rx (rx "(" (* space) (group defn-like) (+ space) (group sym) symbol-end))
  (defconst langsam--defn-rx (rx symbol-start defn-like symbol-end))
  (defconst langsam--fn*-rx (rx "(" (* space) (group fn-like) (+ space) (group sym) symbol-end))
  (defconst langsam--fn-rx (rx symbol-start fn-like symbol-end))
  (defconst langsam--call-rx (rx "(" (* space) (group sym) symbol-end))
  (defconst langsam--builtin-rx (rx symbol-start (or "if" "let" "assert") symbol-end))
  (defconst langsam--symbol-rx (rx symbol-start sym symbol-end)))

(defconst langsam--number-face
  (if (facep 'font-lock-number-face)
      'font-lock-number-face
    'font-lock-constant-face))

(defconst langsam--function-call-face
  (if (facep 'font-lock-function-call-face)
      'font-lock-function-call-face
    'font-lock-function-name-face))

(defconst langsam--font-lock-keywords
  `((,langsam--number-rx . langsam--number-face)
    (,langsam--keyword-rx . font-lock-constant-face)
    (,langsam--opword-rx . font-lock-constant-face)
    (,langsam--nil-rx . font-lock-builtin-face)
    (,langsam--boolean-rx . font-lock-builtin-face)
    (,langsam--typename-rx . font-lock-type-face)
    (,langsam--def*-rx
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face))
    (,langsam--def-rx . font-lock-keyword-face)
    (,langsam--defn*-rx
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,langsam--defn-rx . font-lock-keyword-face)
    (,langsam--fn*-rx
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face))
    (,langsam--fn-rx . font-lock-keyword-face)
    (,langsam--call-rx
     (1 langsam--function-call-face))
    (,langsam--builtin-rx . font-lock-keyword-face)))

;;;; REPL bridge ---------------------------------------------------------------

(defvar langsam-repl-buffer "*Langsam REPL*")

(defcustom langsam-repl-prompt-regexp "^@L\\(:[^>\n]+\\)?> *"
  "Regexp that matches the Langsam REPL prompt."
  :type 'regexp :group 'langsam)

(define-derived-mode langsam-repl-mode comint-mode "Langsam-REPL"
  "Mode for interacting with a Langsam REPL."
  (setq-local comint-prompt-regexp langsam-repl-prompt-regexp)
  (setq-local comint-use-prompt-regexp t)
  (setq-local comint-prompt-read-only t)
  (when (fboundp 'ansi-color-for-comint-mode-on)
    (ansi-color-for-comint-mode-on)))

(defun langsam--repl-process ()
  "Return the Langsam REPL process or signal a user error."
  (or (get-buffer-process langsam-repl-buffer)
      (user-error "No Langsam REPL process found")))

(defun langsam-repl ()
  "Start or switch to the Langsam REPL."
  (interactive)
  (unless (get-buffer-process langsam-repl-buffer)
    (apply #'make-comint-in-buffer "langsam" langsam-repl-buffer
           langsam-repl-executable nil nil))
  (pop-to-buffer langsam-repl-buffer)
  (langsam-repl-mode))

(defun langsam--skip-trivia-backward ()
  "Move point backward over trailing whitespace/comments."
  (skip-chars-backward " \t\n\r")
  ;; If point is inside a comment or string, jump to its start and keep going.
  (let (moved)
    (while
        (let* ((ppss (syntax-ppss))
               (in-str (nth 3 ppss))
               (in-com (nth 4 ppss)))
          (cond
           (in-str  (goto-char (nth 8 ppss)) (setq moved t))
           (in-com  (goto-char (nth 8 ppss)) (setq moved t))
           (t nil)))
      (skip-chars-backward " \t\n\r"))
    moved))

;;;; ---- langsam-eval-capture.el ----

(defun langsam--send-and-capture (code)
  "Send CODE to Langsam REPL and capture output until the next prompt.
Returns the response as a string."
  (with-temp-buffer
    (let* ((redir (current-buffer))
           (proc (langsam--repl-process)))
      (comint-redirect-send-command-to-process code redir proc t t)
      ;; Wait until comint signals completion.
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-redisplay t))
          (while (null comint-redirect-completed)
            (accept-process-output proc 0.05))))
      ;; Grab the text and clean it up.
      (goto-char (point-min))
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun langsam--bounds-of-last-sexp ()
  "Return (BEG . END) of the last complete sexp before point, or nil."
  (save-excursion
    (langsam--skip-trivia-backward)
    (let* ((end (point))
           (beg (ignore-errors (scan-sexps end -1))))
      (when beg (cons beg end)))))

(defun langsam-send-last-sexp-and-show ()
  "Send last sexp, capture its output, and show it.
Displays a one-line preview in the minibuffer and an overlay at point."
  (interactive)
  (let* ((bounds (or (langsam--bounds-of-last-sexp)
                     (user-error "No complete sexp found before point")))
         (code (buffer-substring-no-properties (car bounds) (cdr bounds)))
         ;; Append newline to actually trigger eval in most REPLs:
         (resp (langsam--send-and-capture (concat code "\n"))))
    (message "%s" resp)
    ;; Optional: inline overlay result after the sexp
    (let* ((ov (make-overlay (cdr bounds) (cdr bounds)))
           (text (concat " ⇒ " resp)))
      (overlay-put ov 'after-string (propertize text 'face 'shadow))
      (redisplay)
      (let ((evt (read-event)))
        (when (overlayp ov) (delete-overlay ov))
        (push evt unread-command-events)))))

(defun langsam-send-region (beg end)
  "Send region between BEG and END to REPL."
  (interactive "r")
  (let ((proc (langsam--repl-process)))
    (comint-send-region proc beg end)
    (comint-send-string proc "\n")))

(defun langsam-send-defun ()
  "Send top-level form to REPL."
  (interactive)
  (save-mark-and-excursion
    (let ((inhibit-message t))
      (mark-defun))
    (langsam-send-region (region-beginning) (region-end))))

;;;; Mode definition -----------------------------------------------------------

(defvar langsam-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-z") #'langsam-repl)
    (define-key m (kbd "C-c C-c") #'langsam-send-defun)
    (define-key m (kbd "C-c C-r") #'langsam-send-region)
    (define-key m (kbd "C-x C-e") #'langsam-send-last-sexp-and-show)
    m))

(defvar langsam-indent-specs
  '((def . defun) (defn . defun) (defmacro . defun)
    (defmulti . defun) (defmethod . defun)
    (fn . defun) (macro . defun)
    (let . 1)
    (if . 1) (when . 1) (unless . 1)
    (case . 1) (doseq . 1) (for . 1)
    (cond . 0) (do . 0))
  "Indent rules for Langsam forms.
Value `defun` means defun-style indentation (name + args special).
An integer N means: indent the first N forms by one step; rest align.")

(defun langsam-indent-function (indent-point state)
    "Indent function for Langsam forms (Clojure-like).
Respects `langsam-indent-specs` and otherwise falls back to `lisp-indent-function`."
    (let* ((normal-indent (current-column))
           (delim-pos (elt state 1))
           (delim (char-after delim-pos))
           (call? (eq delim ?\())
           (head (and call? (save-excursion
                              (goto-char (1+ delim-pos))
                              (intern-soft (thing-at-point 'symbol t)))))
           (last-char (and call? (save-excursion
                                   (goto-char delim-pos)
                                   (end-of-line)
                                   (char-before (point)))))
           (spec (cdr (assoc head langsam-indent-specs))))
      (cond
       ;; defun-style (name + arglist get special treatment)
       ((or (eq spec 'defun) (eq last-char ?\]))
        (lisp-indent-defform state indent-point))

       ;; first N arguments are “special” (e.g., let: 1 = bindings)
       ((and (integerp spec) (>= spec 0))
        (lisp-indent-specform spec state indent-point normal-indent))

       ;; vectors and maps
       ((memq delim '(?\[ ?\{))
        (save-excursion
          (goto-char (1+ delim-pos))
          (skip-syntax-forward " -") ; skip spaces/comments
          (current-column)))

       ;; otherwise, use the stock Lisp rules
       (t (lisp-indent-function indent-point state)))))

;;;###autoload
(define-derived-mode langsam-mode prog-mode "Langsam"
  "Major mode for editing Langsam code."
  :syntax-table langsam-mode-syntax-table

  ;; Font-lock
  (setq-local font-lock-defaults `(langsam--font-lock-keywords))

  ;; Indentation
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-indent-function #'langsam-indent-function)
  (setq-local indent-tabs-mode nil)

  ;; Comments
  (setq-local parse-sexp-ignore-comments t)

  (setq-local comment-use-syntax t)
  (setq-local comment-start ";")
  (setq-local comment-end ""))

(provide 'langsam-mode)
;;; langsam-mode.el ends here
