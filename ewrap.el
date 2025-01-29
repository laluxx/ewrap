;;; ewrap.el --- Smart wrapping of Lisp forms -*- lexical-binding: t -*-

;; Copyright (C) 2025 Laluxx

;; Author: Laluxx
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience, lisp
;; URL: https://github.com/laluxx/ewrap

;;; Commentary:

;; `ewrap' provides smart wrapping functionality for Lisp forms.
;; It understands function arities and special forms, making it easier
;; to manipulate Lisp code.

;; Features:
;; - Smart wrapping of symbols based on function arity
;; - Special handling of 'if' forms
;; - Undo support for quick corrections

;;; Code:

(defgroup ewrap nil
  "Smart wrapping of Lisp forms."
  :group 'editing
  :prefix "ewrap-")

(defcustom ewrap-excluded-functions
  '(lambda interactive)
  "Functions to exclude from arity-based wrapping due to special handling needs."
  :type '(repeat symbol)
  :group 'ewrap)

(defun ewrap-get-function-arity (sym)
  "Get the minimum arity of function SYM, or nil if not applicable."
  (when (and sym
             (functionp sym)
             (not (memq sym ewrap-excluded-functions)))
    (car (func-arity sym))))

(defun ewrap-unwrap-if ()
  "Remove if form at point, keeping the true branch."
  (let ((if-start (point)))
    (forward-sexp)  ; Move past entire if form
    (let ((if-end (point)))
      ;; Parse the if form to extract the true branch
      (goto-char if-start)
      (down-list)
      (forward-sexp 2)  ; Skip 'if' and condition
      (forward-sexp)    ; Get to end of true branch
      (let* ((true-end (point))
             (true-start (save-excursion
                           (backward-sexp)
                           (point)))
             (true-body (buffer-substring-no-properties true-start true-end)))
        (delete-region if-start if-end)
        (insert true-body)))))

(defun ewrap-wrap-with-args (bounds min-args)
  "Wrap region defined by BOUNDS with parentheses, handling MIN-ARGS required arguments."
  (condition-case nil
      (let ((start (car bounds)))
        (goto-char (cdr bounds))       ; End of function name
        (dotimes (_ min-args)          ; Try to find all required args
          (forward-sexp))
        (insert ")")
        (goto-char start)
        (insert "("))
    (error                            ; Missing args or invalid syntax
     (goto-char (car bounds))
     (insert "(")
     (goto-char (cdr bounds))
     (insert ")"))))

(defun ewrap-wrap-symbol ()
  "Wrap symbol at point, considering function arity if applicable."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (sym (when bounds 
                (intern-soft (buffer-substring-no-properties 
                              (car bounds) (cdr bounds)))))
         (min-args (ewrap-get-function-arity sym)))
    (if (and min-args                     ; It's a function
             (not (nth 3 (syntax-ppss)))) ; Not in a string
        (ewrap-wrap-with-args bounds min-args)
      ;; Not a function or in a string - wrap word
      (skip-syntax-backward "w")
      (insert "(")
      (skip-syntax-forward "w")
      (insert ")"))))

;;;###autoload
(defun ewrap ()
  "Smart wrapping of Lisp forms using Emacs' built-in form understanding.
When called on an opening parenthesis of an if form, removes the if form.
Otherwise, wraps the symbol at point with parentheses, considering function arity."
  (interactive)
  (if (eq last-command 'ewrap)
      (undo)
    (if (and (looking-at "(")
             (save-excursion
               (down-list)
               (looking-at "if\\s-")))
        (save-excursion (ewrap-unwrap-if))
      (condition-case nil
          (delete-pair)
        (error
         (save-excursion
           (ewrap-wrap-symbol)))))))

(provide 'ewrap)
;;; ewrap.el ends here
