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

(defun phony--strip-rule-prefix (name)
  "Strip the rule/ prefix from NAME if present."
  ;; This should be temporary
  (intern (string-remove-prefix (symbol-name name))))

(defun phony--completion-at-point ()
  (and (phony--in-element-form-p)
       (and-let* ((bounds (bounds-of-thing-at-point 'symbol)))
         (list (car bounds)
               (cdr bounds)
               (seq-map (lambda (rule)
                          (phony--strip-rule-prefix (phony--rule-name rule)))
                        (phony--get-rules))
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

(defun phony--find-definition (name)
  (re-search-forward
   (rx (seq (or "(phony-defun"
                "(phony-define-dictionary"
                "(phony-define-open-rule")
            (+ (not graphic))
            (group (literal (symbol-name name)))
            symbol-end))
   nil t))

(require 'find-func)

(setf (alist-get 'phony find-function-regexp-alist)
      #'phony--find-definition)

(defun phony--xref-backend-definitions (f backend identifier)
  (if (or (not (eq backend 'elisp))
          (not (phony--in-element-form-p
                (get-text-property 0 'pos identifier))))
      (funcall f backend identifier)
    (and-let* ((rule-name (intern-soft identifier))
               (rule (phony--get-rule rule-name))
               (file-name (phony--rule-file-name rule))
               (location (find-function-search-for-symbol
                          rule-name
                          'phony
                          file-name))
               (buffer (car location))
               (position (cdr location)))
      (with-current-buffer buffer
        (save-restriction
          (widen)
          (save-excursion
            (goto-char position)
            (list
             (xref-make
              identifier
              (xref-make-file-location
               file-name
               (line-number-at-pos nil t)
               (- (point) (line-beginning-position)))))))))))

(advice-add #'xref-backend-definitions :around #'phony--xref-backend-definitions)

(provide 'phony-experimental)
;;; phony-experimental.el ends here
