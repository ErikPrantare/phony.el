;;; phony-serialize-syntax.el --- Write grammar syntax to a file -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides `phony--write-syntax-to-file' for writing the
;; currently loaded syntax to a file.  `phony--read-syntax-from-file'
;; reads a such syntax file, and returns a `phony--grammar'.

;; This only serializes syntax.  Procedure rule definitions and
;; dictionary values are removed.

;; The ability to serialize syntax is useful for generating inputs for
;; benchmarking purposes.

;;; Code:

(require 'phony)

(defun phony--write-syntax-to-file (file)
  "Write the currently loaded syntax to FILE.

This will unparse the current grammar, stripping any semantics like
dictionary values or procedure rule definitions."
  (with-temp-file file
    (let* ((grammar phony--default-grammar)
           (rules (phony--get-rules grammar))
           (modules (map-values (phony--grammar-modules grammar))))
      (setq rules (seq-sort-by #'phony--rule-name #'string< rules))
      (setq modules (seq-sort-by #'phony--module-name #'string< modules))
      (insert ";; Generated with `phony--write-syntax-to-file'\n")
      (dolist (module modules)
        (insert (format "%S\n" (phony--unparse module))))
      (maphash (lambda (interface provider)
                 (insert (format
                          "%S\n"
                          `(phony-provide-from ,interface ,provider))))
               (phony--grammar-module-provision grammar))
      (dolist (rule rules)
        (insert (format "%S\n" (phony--unparse rule)))))))

(defun phony--read-syntax-from-file (file)
  "Return a `phony--grammar' object of the syntax in FILE.
FILE must have been written to using `phony--write-syntax-to-file'.

WARNING: This evaluates the contents of FILE as Lisp.  Do not read
syntax from untrusted sources!"
  (let ((phony--default-grammar (phony--make-grammar)))
    (load file)
    phony--default-grammar))

(cl-defgeneric phony--unparse (node)
  "Unparse NODE into a sexp.")

(cl-defmethod phony--unparse ((module phony--module))
  "Unparse MODULE."
  `(phony-define-module ,(phony--module-name module)))

(cl-defgeneric phony--rule-parameters (rule)
  "Return keyword parameters of RULE as a plist.")

(cl-defmethod phony--rule-parameters ((rule phony--rule))
  "Return keyword parameters of RULE."
  `(,@(and-let* ((modes (phony--rule-modes rule))
                 ((not (memq 'global modes))))
        `(:mode ,modes))
    ,@(and-let* ((module (phony--rule-module rule)))
        `(:module ,module))
    ,@(and-let* ((contributes-to (phony--rule-contributes-to rule)))
        `(:contributes-to ,contributes-to))
    ,@(and-let* ((provides (phony--rule-provides rule)))
        `(:provides ,provides))))

(cl-defmethod phony--rule-parameters ((rule phony--procedure-rule))
  "Return keyword parameters of RULE."
  `( :interactive ,(phony--procedure-rule-interactive-p rule)
     ,@(and (phony--procedure-rule-anchor-beginning-p rule)
            '(:anchor-beginning t))
     ,@(and (phony--procedure-rule-anchor-end-p rule)
            '(:anchor-end t))
     ,@(cl-call-next-method)))

(cl-defmethod phony--rule-parameters ((rule phony--open-rule))
  "Return keyword parameters of RULE."
  `(,@(and-let* ((alternatives (phony--open-rule-alternatives rule)))
        `(:alternatives ,alternatives))
    ,@(cl-call-next-method)))

(cl-defmethod phony--rule-parameters ((rule phony--interface-rule))
  "Return keyword parameters of RULE."
  `(,@(and-let* ((default (phony--interface-rule-default rule)))
        `(:default ,default))
    ,@(cl-call-next-method)))

(cl-defmethod phony--unparse ((rule phony--procedure-rule))
  "Unparse RULE."
  `(phony-defun ,(phony--rule-name rule)
       ,(mapcar #'phony--unparse
                (phony--pattern-sequence-patterns
                 (phony--procedure-rule-pattern rule)))
     ,@(phony--rule-parameters rule)
     ;; Refer to args so as to not trigger unused argument warnings.
     (ignore ,@(phony--procedure-rule-arglist rule))))

(cl-defmethod phony--unparse ((rule phony--dictionary))
  "Unparse RULE."
  `(phony-define-dictionary ,(phony--rule-name rule)
     ,@(phony--rule-parameters rule)
     ;; Scrub values and order entries
     ',(sort (mapcar (lambda (entry) (cons (car entry) nil))
                     (phony--dictionary-mapping rule))
             :key #'car :lessp #'string<)))

(cl-defmethod phony--unparse ((rule phony--open-rule))
  "Unparse RULE."
  `(phony-define-open-rule ,(phony--rule-name rule)
     ,@(phony--rule-parameters rule)))

(cl-defmethod phony--unparse ((rule phony--interface-rule))
  "Unparse RULE."
  `(phony-define-interface ,(phony--rule-name rule)
     ,@(phony--rule-parameters rule)))

(cl-defmethod phony--unparse ((pattern phony--pattern-literal))
  "Unparse PATTERN."
  (phony--pattern-literal-string pattern))

(cl-defmethod phony--unparse ((pattern phony--pattern-rule))
  "Unparse PATTERN."
  (phony--pattern-rule-name pattern))

(cl-defmethod phony--unparse ((_pattern phony--pattern-builtin-word))
  "Unparse PATTERN."
  'phony--builtin-word)

(cl-defmethod phony--unparse ((_pattern phony--pattern-builtin-phrase))
  "Unparse PATTERN."
  'phony--builtin-phrase)

(cl-defmethod phony--unparse ((pattern phony--pattern-argument))
  "Unparse PATTERN."
  `(,(phony--pattern-argument-name pattern)
    ,(phony--unparse (phony--pattern-argument-pattern pattern))))

(cl-defmethod phony--unparse ((pattern phony--pattern-external-rule))
  "Unparse PATTERN."
  `(external-rule ,@(phony--pattern-external-rule-namespace pattern)
                  ,(phony--pattern-external-rule-name pattern)))

(cl-defmethod phony--unparse ((pattern phony--pattern-sequence))
  "Unparse PATTERN."
  `(seq ,@(mapcar #'phony--unparse
                  (phony--pattern-sequence-patterns pattern))))

(cl-defmethod phony--unparse ((pattern phony--pattern-optional))
  "Unparse PATTERN."
  `(\? ,(phony--unparse
         (phony--pattern-optional-pattern pattern))))

(cl-defmethod phony--unparse ((pattern phony--pattern-one-or-more))
  "Unparse PATTERN."
  `(+ ,(phony--unparse
        (phony--pattern-repeat-pattern pattern))))

(cl-defmethod phony--unparse ((pattern phony--pattern-zero-or-more))
  "Unparse PATTERN."
  `(* ,(phony--unparse
        (phony--pattern-repeat-pattern pattern))))

(provide 'phony-serialize-syntax)
;;; phony-serialize-syntax.el ends here
