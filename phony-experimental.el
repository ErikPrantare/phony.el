;;; phony-experimental.el --- Experimental features for phony  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'phony)
(require 'xref)

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
  (or
   (seq-contains-p (phony--function-calls-at position)
                   '(phony-defun 2))
   (seq-contains-p (phony--function-calls-at position)
                   '(phony--evaluate-ast 1))))

(defun phony--completion-at-point ()
  (and (phony--in-element-form-p)
       (and-let* ((bounds (bounds-of-thing-at-point 'symbol)))
         (list (car bounds)
               (cdr bounds)
               (seq-map #'phony--rule-name (phony--get-rules))
               :exclusive 'yes))))

(defun phony--enable-capf ()
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions
               #'phony--completion-at-point))

(defun phony--disable-capf ()
  (setq completion-at-point-functions
        (delq #'phony--completion-at-point
              completion-at-point-functions)))

(defvar phony--font-lock-keywords
  `((,(rx "(" (or "phony-defun"
                  "phony-define-dictionary"
                  "phony-define-open-rule")
          (+ blank)
          (group (+ (or (syntax word) (syntax symbol)))))
     1 'font-lock-function-name-face)))

(defun phony--enable-font-lock ()
  (font-lock-add-keywords nil phony--font-lock-keywords))

(defun phony--disable-font-lock ()
  (font-lock-remove-keywords nil phony--font-lock-keywords))

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

(cl-defmethod xref-backend-definitions ((_backend (eql 'phony)) identifier)
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
             (- (point) (line-beginning-position))))))))))

(defun phony--xref-backend ()
  (and (phony--in-element-form-p) 'phony))

(defun phony--enable-xref ()
  (add-hook 'xref-backend-functions #'phony--xref-backend nil t))

(defun phony--disable-xref ()
  (remove-hook 'xref-backend-functions #'phony--xref-backend t))

(defun phony--enable-experimental-features ()
  (phony--enable-capf)
  (phony--enable-xref)
  (phony--enable-font-lock))

(defun phony--disable-experimental-features ()
  (phony--disable-capf)
  (phony--disable-xref)
  (phony--disable-font-lock))

(defun phony--install-experimental-features ()
  (unless phony-mode
    (setq phony-experimental-mode nil)
    (error "Enable `phony-mode' before enabling `phony-experimental-mode'."))
  (dolist (buffer (match-buffers (cons 'derived-mode 'emacs-lisp-mode)))
    (with-current-buffer buffer
      (phony--enable-experimental-features)))
  (add-hook 'emacs-lisp-mode-hook #'phony--enable-experimental-features)
  (setf (alist-get 'phony find-function-regexp-alist) #'phony--find-definition))

(defun phony--uninstall-experimental-features ()
  (dolist (buffer (match-buffers (cons 'derived-mode 'emacs-lisp-mode)))
    (with-current-buffer buffer
      (phony--disable-experimental-features)))
  (remove-hook 'emacs-lisp-mode-hook #'phony--enable-experimental-features)
  (setf (alist-get 'phony find-function-regexp-alist nil t) nil))

(define-minor-mode phony-experimental-mode
  "Toggle experimental features for phony.

This includes `completion-at-point', `xref', and font-locking."
  :global t
  :group 'phony
  (if phony-experimental-mode
      (phony--install-experimental-features)
    (phony--uninstall-experimental-features)))

(provide 'phony-experimental)
;;; phony-experimental.el ends here
