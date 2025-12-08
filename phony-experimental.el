;;; phony-experimental.el --- Experimental features for phony  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'phony)

(defmacro phony-defun (name pattern &rest rest)
  (declare (indent 2)
           (doc-string 3))
  ;; Tooling that we want to work before we are ready to ship this:
  ;; - xref: DONE
  ;; - completion-at-point: DONE
  ;; - syntax highlighting for keywords

  ;; Other todos:
  ;; - Do not hijack the function namespace.  The tooling may need to
  ;;   be revisited after this.
  ;; - Check TODOs for better error messages.
  ;; - Migrate old rule kinds with rule/ prefixes.

  ;; Syntactical design decisions:
  ;; - Automatic argument deduction: Explicitly specifying the
  ;;   argument list meant that adding a parameter necessitated a
  ;;   change in two places.  Things like optional arguments were not
  ;;   semantically relevant in the argument list, as they would
  ;;   always be populated with nil anyway.
  ;; - Optional parameters after docstring: The doc-string declare
  ;;   form only allows a constant, so comment syntax highlighting
  ;;   would not be possible the other way around.
  ;; - Elision of parentheses when pattern is just a literal: Allows the
  ;;   following nice looking syntax:
  ;;   (phony-defun chuck-line "chuck line"
  ;;     (kill-line))
  ;; - Pattern after name, before doc-string:
  ;;   - I want the pattern to be indented specially, but not for
  ;;     documentation.  The indent declare form allows this if
  ;;     pattern is before documentation.
  ;;   - If it was the other way around and documentation was omitted,
  ;;     the elision of parentheses for string literals could trick
  ;;     the syntax highlighting to treat the test documentation.
  ;; - Implicit argument naming: This allows the following terse
  ;;   syntax for simple rules:
  ;;   (phony-defun chuck-thing ("chuck" thing)
  ;;     (kill-thing thing))
  ;;   This will be useful when rules don't share namespace with
  ;;   functions anymore, allowing shorter names.
  ;; - Name phony-defun: Chosen because of the similarity with defun.
  ;;   However, this could potentially give the impression that we are
  ;;   defining a function, something that may not be true in the future.
  ;; - Implicit seq for sequence of elements: In practice, this is the
  ;;   most common construction to use at the top level (perhaps
  ;;   excepts a single string literal, which is also covered by
  ;;   shorthand).  Might as well allow eliding it.
  ;; - Unquoted :contributes-to: By restricting the legal forms that
  ;;   may be passed to it, we can better utilize capf and xref.  For
  ;;   example, we know that function names cannot appear in the form,
  ;;   so those may be filtered out from completion and
  ;;   go-to-definition.
  (let* ((doc (and (stringp (car rest)) (pop rest)))
         (split-arguments (phony--split-keywords-rest rest))
         (optional-arguments (car split-arguments))
         (body (cdr split-arguments))
         (expanded-pattern
          (phony--internalize-rules
           (phony--expand-implicit-arguments
            (cons 'seq (ensure-list pattern)))))
         (arguments (phony--collect-arguments expanded-pattern)))

    (cond
     ((not (seq-every-p (lambda (argument) (memq argument (flatten-tree body)))
                        arguments))
      ;; TODO: Which arguments?
      `(display-warning
        'phony
        ,(format "Not all arguments used in body of %s" name)))
     ((not (equal (seq-uniq arguments) arguments))
      ;; TODO: Do not emit this warning if duplication is implicit and
      ;; not used in body.
      `(display-warning
        'phony
        ,(format (concat "Duplicate arguments in %s\n"
                         "If you use implicit argument names, make them explicit")
                 name)))
     (t `(defun ,(phony--internal-name name) ,arguments
           ,@(ensure-list doc)
           (declare (phony-rule
                     ,@optional-arguments
                     ,expanded-pattern))
           ,@body)))))

(defun phony--internal-name (name)
  ;; For now, we will assume the convention of prefixing rules with
  ;; rule/.  Eventually, we will want to remove the rules from the
  ;; function namespace, at which point this will be superfluous.
  (intern (concat "rule/" (symbol-name name))))

(defun phony--internalize-rules (element-form)
  (cond
   ((and (listp element-form) (memq (car element-form) '(seq ? + *)))
    `(,(car element-form)
      ,@(seq-map #'phony--internalize-rules (cdr element-form))))
   ((and (listp element-form) (symbolp (cadr element-form)))
    `(,(car element-form) ,(phony--internal-name (cadr element-form))))
   (t element-form)))

(defun phony--expand-implicit-arguments (element-form)
  (cond
   ((and (listp element-form) (memq (car element-form) '(seq ? + *)))
    `(,(car element-form)
      ,@(seq-map #'phony--expand-implicit-arguments (cdr element-form))))
   ((symbolp element-form)
    `(,element-form ,element-form))
   (t element-form)))

(defun phony--collect-arguments (element-form)
  (cond
   ((and (listp element-form) (memq (car element-form) '(seq ? + *)))
    (seq-mapcat #'phony--collect-arguments (cdr element-form)))
   ((listp element-form)
    (list (car element-form)))
   (t '())))

(defun phony--function-calls-at (&optional position)
  (unless position (setq position (point)))
  (save-excursion
    (goto-char position)
    (let ((functions '()))
      ;; We walk up the chain of function calls, until
      ;; backward-up-list yields an error.  At that point, we are at
      ;; top level.
      (condition-case nil
          (while t
            (push (elisp--fnsym-in-current-sexp) functions)
            (backward-up-list))
        (error functions)))))

(defun phony--in-element-form-p (&optional position)
  (unless position (setq position (point)))
  (seq-contains-p (phony--function-calls-at position) '(phony-defun 2)))

(defun phony--completion-at-point ()
  (and (phony--in-element-form-p)
       (and-let* ((bounds (bounds-of-thing-at-point 'symbol)))
         (list (car bounds)
               (cdr bounds)
               (seq-map #'phony--rule-name (phony--get-rules))
               :exclusive 'yes))))

(defun phony--install-capf ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'phony--completion-at-point))

(add-hook 'emacs-lisp-mode-hook #'phony--install-capf)

(defun phony--install-font-lock ()
  (font-lock-add-keywords
   nil
   `((,(rx "(" (or "phony-defun"
                   "phony-define-dictionary"
                   "phony-define-open-rule")
           (+ blank)
           (group (+ (or (syntax word) (syntax symbol)))))
      1 'font-lock-function-name-face))))

(add-hook 'emacs-lisp-mode-hook #'phony--install-font-lock)

(defun phony--infer-namespace-advice (position)
  (and (phony--in-element-form-p position)
       'phony))

(advice-add #'elisp--xref-infer-namespace :before-until
            #'phony--infer-namespace-advice)

(defun phony--find-definitions-advice (f symbol)
  (if-let ((rule-name (intern-soft
                       (concat "rule/" (symbol-name symbol)))))
      (append (funcall f rule-name)
              (funcall f symbol))
    (funcall f symbol)))

(advice-add #'elisp--xref-find-definitions :around
            #'phony--find-definitions-advice)

(defun phony--filter-definitions-advice (definitions namespace symbol)
  (and (eq namespace 'phony)
       (let ((rule-name (intern (concat "rule/" (symbol-name symbol)))))
         (seq-filter (lambda (item)
                       (eq (xref-elisp-location-symbol
                            (xref-item-location item))
                           rule-name))
                     definitions))))

(advice-add #'elisp--xref-filter-definitions :before-until
            #'phony--filter-definitions-advice)

(provide 'phony-experimental)
;;; phony-experimental.el ends here
