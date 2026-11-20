;;; phony-pattern.el --- Patterns of procedure rules -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
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

;; This file specifies the structure, syntax and some helpers for the
;; patterns, used in `phony-defun'.

;;; Code:

(require 'cl-lib)

(cl-defstruct (phony-pattern--literal
               (:constructor phony-pattern--make-literal))
  "Pattern matching a literal utterance."
  ( string nil
    :type string
    :documentation "Utterance that matches this pattern."))

(cl-defstruct (phony-pattern--sequence
               (:constructor phony-pattern--make-sequence))
  "Pattern matching a sequence of sub-patterns."
  ( patterns nil
    :documentation "List of sub-patterns that must match in sequence."))

(cl-defstruct (phony-pattern--optional
               (:constructor phony-pattern--make-optional))
  "Pattern matching PATTERN zero or one times."
  ( pattern nil
    :documentation "The pattern to match zero or one times."))

(cl-defstruct (phony-pattern--repeat
               (:constructor phony-pattern--make-repeat))
  "Pattern that may match PATTERN multiple times."
  ( pattern nil
    :documentation "The pattern to match repeatedly."))

(cl-defstruct (phony-pattern--one-or-more
               (:include phony-pattern--repeat)
               (:constructor phony-pattern--make-one-or-more))
  "Pattern matching PATTERN one or more times.")

(cl-defstruct (phony-pattern--zero-or-more
               (:include phony-pattern--repeat)
               (:constructor phony-pattern--make-zero-or-more))
  "Pattern matching PATTERN zero or more times.")

(cl-defstruct (phony-pattern--alternative
               (:constructor phony-pattern--make-alternative))
  "Pattern matching one of PATTERNS."
  ( patterns nil
    :documentation "The alternatives to match."))

(cl-defstruct (phony-pattern--argument
               (:constructor phony-pattern--make-argument))
  "Pattern matching FORM and binding it to function argument NAME."
  ( name nil
    :type symbol
    :documentation "Symbol naming the argument that captures the value of the match.")
  (pattern nil
           :type sexp
           :documentation "Pattern whose match will bind to the argument."))

(cl-defstruct (phony-pattern--rule
               (:constructor phony-pattern--make-rule))
  "Pattern matching another rule."
  ( reference nil
    :type phony--rule-reference
    :documentation "Reference to the rule."))

(cl-defstruct (phony-pattern--builtin-word
               (:constructor phony-pattern--make-builtin-word))
  "Pattern matching any word.")

(cl-defstruct (phony-pattern--builtin-phrase
               (:constructor phony-pattern--make-builtin-phrase))
  "Pattern matching any phrase.")

;; TODO: We should factor out the grammar AST datastructure into one
;; file.  The following structure is awkwardly placed.
(cl-defstruct (phony--rule-reference
               (:constructor phony--make-rule-reference)
               (:copier phony--copy-rule-reference))
  "A reference to a rule."
  ( name nil
    :type symbol
    :documentation "The name of the referenced rule."))

(defun phony-pattern--rule-name (pattern)
  "Return the name of the rule referenced by PATTERN."
  (phony--rule-reference-name (phony-pattern--rule-reference pattern)))

(defun phony-pattern--parse-implicit-sequence (pattern-forms arglist)
  "Parse implicit sequence form PATTERN-FORMS.

An implicit sequence form is one where the symbol `seq' is not
explicitly specified, as for ?, + and *.

If PATTERN-FORMS has one element, that form is parsed and returned
directly.  Otherwise, a sequence pattern containing the parse of
PATTERN-FORMS is returned.

ARGLIST is the same as for `phony-pattern--parse'."
  (if (length= pattern-forms 1)
      (phony-pattern--parse (car pattern-forms) arglist)
    (phony-pattern--make-sequence
     :patterns (mapcar
                (lambda (pattern-form)
                  (phony-pattern--parse
                   pattern-form arglist))
                pattern-forms))))

(defun phony-pattern--parse (pattern-form arglist)
  "Parse PATTERN-FORM into an pattern.

ARGLIST is the list of argument names for the function being defined.
This is required for recognizing if a form should bind to an argument."
  (cond
   ((stringp pattern-form)
    (phony-pattern--make-literal
     :string pattern-form))
   ((member (car-safe pattern-form) arglist)
    (phony-pattern--make-argument
     :name (car pattern-form)
     :pattern (phony-pattern--parse
               (cadr pattern-form) arglist)))
   ;; NOTE: The reader interprets ? as a character escape, so to use
   ;; it in the specification of the pattern we actually need to
   ;; match on the character after that, which we require to be
   ;; space.
   ((or (eq (car-safe pattern-form) ?\s)
        (eq (car-safe pattern-form) '\?))
    (phony-pattern--make-optional
     :pattern
     (phony-pattern--parse-implicit-sequence
      (cdr pattern-form) arglist)))
   ((eq (car-safe pattern-form) '+)
    (phony-pattern--make-one-or-more
     :pattern
     (phony-pattern--parse-implicit-sequence
      (cdr pattern-form) arglist)))
   ((eq (car-safe pattern-form) '*)
    (phony-pattern--make-zero-or-more
     :pattern
     (phony-pattern--parse-implicit-sequence
      (cdr pattern-form) arglist)))
   ((eq (car-safe pattern-form) 'seq)
    (phony-pattern--make-sequence
     :patterns
     (mapcar
      (lambda (subpattern)
        (phony-pattern--parse subpattern arglist))
      (cdr pattern-form))))
   ((eq (car-safe pattern-form) '|)
    (phony-pattern--make-alternative
     :patterns
     (mapcar
      (lambda (subpattern)
        (phony-pattern--parse subpattern arglist))
      (cdr pattern-form))))
   ((eq pattern-form 'phony--builtin-word)
    (phony-pattern--make-builtin-word))
   ((eq pattern-form 'phony--builtin-phrase)
    (phony-pattern--make-builtin-phrase))
   ((symbolp pattern-form)
    (phony-pattern--make-rule
     :reference (phony--make-rule-reference
                 :name pattern-form)))
   ((symbolp (car pattern-form))
    (error "Not an argument nor pattern type: `%S' in form `%S'"
           (car pattern-form)
           pattern-form))
   (t (error "No parse for %S" pattern-form))))

(defun phony-pattern--map-children (f pattern)
  "Return a copy of PATTERN with F applied to each of its children."
  (pcase pattern
    ((cl-struct phony-pattern--sequence patterns)
     (phony-pattern--make-sequence
      :patterns (seq-map f patterns)))
    ((cl-struct phony-pattern--alternative patterns)
     (phony-pattern--make-alternative
      :patterns (seq-map f patterns)))
    ((cl-struct phony-pattern--optional (pattern child))
     (phony-pattern--make-optional
      :pattern (funcall f child)))
    ((cl-struct phony-pattern--one-or-more (pattern child))
     (phony-pattern--make-one-or-more
      :pattern (funcall f child)))
    ((cl-struct phony-pattern--zero-or-more (pattern child))
     (phony-pattern--make-zero-or-more
      :pattern (funcall f child)))
    ((cl-struct phony-pattern--argument name (pattern child))
     (phony-pattern--make-argument
      :name name
      :pattern (funcall f child)))
    ((cl-struct phony-pattern--literal string)
     (phony-pattern--make-literal :string string))
    ((cl-struct phony-pattern--rule reference)
     (phony-pattern--make-rule
      :reference (phony--copy-rule-reference reference)))
    ((cl-struct phony-pattern--builtin-word)
     (phony-pattern--make-builtin-word))
    ((cl-struct phony-pattern--builtin-phrase)
     (phony-pattern--make-builtin-phrase))
    (_ (error "No case for pattern of type %s: %s"
              (type-of pattern)
              pattern))))

(defun phony-pattern--transform (f pattern)
  "Recursively apply F to PATTERN.

F is applied to a copy of PATTERN with children transformed by F."
  (funcall f (phony-pattern--map-children
              (lambda (child) (phony-pattern--transform f child))
              pattern)))

(defun phony-pattern--copy (pattern)
  "Return a deep copy of PATTERN."
  (phony-pattern--transform #'identity pattern))

(defun phony-pattern--children (pattern)
  "Return all direct sub-patterns of PATTERN."
  (let ((children '()))
    (phony-pattern--map-children
     (lambda (child) (push child children))
     pattern)
    (nreverse children)))

(defun phony-pattern--collect (predicate pattern)
  "Return list of descendants of PATTERN matching PREDICATE.

If PATTERN matches PREDICATE, it will be part of the list.  Matches are
returned in the same order that they occur in PATTERN."
  (let ((collected-children
         (seq-mapcat
          (apply-partially #'phony-pattern--collect predicate)
          (phony-pattern--children pattern))))
    (if (funcall predicate pattern)
        (cons pattern collected-children)
      collected-children)))

(provide 'phony-pattern)
;;; phony-pattern.el ends here
