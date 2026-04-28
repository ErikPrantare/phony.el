;;; phony-element-syntax.el --- Element parsing logic  -*- lexical-binding: t; -*-

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

(require 'cl-lib)

(cl-defstruct phony--element-literal
  "Element matching a literal utterance."
  (string nil
          :type string
          :documentation "Utterance that matches this element."))

(cl-defstruct phony--element-sequence
  "Element matching a sequence of sub-elements."
  (elements nil
            :documentation "List of sub-elements that must match in sequence."))

(cl-defstruct phony--element-optional
  "Element matching ELEMENT zero or one times."
  (element nil
           :documentation "The element to match zero or one times."))

(cl-defstruct phony--element-repeat
  "Element that may match ELEMENT multiple times."
  (element nil
           :documentation "The element to match repeatedly."))

(cl-defstruct (phony--element-one-or-more
               (:include phony--element-repeat))
  "Element matching ELEMENT one or more times.")

(cl-defstruct (phony--element-zero-or-more
               (:include phony--element-repeat))
  "Element matching ELEMENT zero or more times.")

(cl-defstruct phony--element-argument
  "Element matching FORM and binding it to function argument NAME."
  (name nil
        :type symbol
        :documentation "Symbol naming the argument that captures the value of the match.")
  (element nil
           :type sexp
           :documentation "Element whose match will bind to the argument."))

(cl-defstruct phony--element-external-rule
  "Element matching some external rule.
Currently only relevant for the talon exporter."
  (name nil
        :documentation "Symbol naming the external rule.")
  (namespace nil
             :documentation "List of symbols forming the namespace prefix."))

(cl-defstruct phony--element-rule
  "Element matching another rule."
  (name nil
        :type symbol
        :documentation "Symbol naming the phony rule that this element matches."))

(defun phony--parse-speech-value-element (element-form)
  "Parse ELEMENT-FORM into a valued element.

Valued elements are those that get associated to a value when matched.
Only these elements may be bound to arguments."
  (cond
   ((symbolp element-form)
    (make-phony--element-rule
     :name element-form))
   ((eq (car element-form) 'external-rule)
    (make-phony--element-external-rule
     :name (car (last (cdr element-form)))
     :namespace (butlast (cdr element-form))))
   ((symbolp (car element-form))
    (error "Not an argument nor element type: `%S' in form `%S'"
           (car element-form)
           element-form))
   (t (error "No parse for %S" element-form))))

(defun phony--parse-speech-element (element-form arglist)
  "Parse ELEMENT-FORM into an element.

ARGLIST is the list of argument names for the function being defined.
This is required for recognizing if a form should bind turn argument."
  (cond
   ((stringp element-form)
    (make-phony--element-literal
     :string element-form))
   ((member (car-safe element-form) arglist)
    (make-phony--element-argument
     :name (car element-form)
     :element (phony--parse-speech-value-element
               (cadr element-form))))
   ;; NOTE: The reader interprets ? as a character escape, so to use
   ;; it in the specification of the pattern we actually need to
   ;; match on the character after that, which we require to be
   ;; space.
   ((eq (car-safe element-form) ?\s)
    (make-phony--element-optional
     :element
     (make-phony--element-sequence
      :elements (mapcar
                 (lambda (subelement)
                   (phony--parse-speech-element
                    subelement arglist))
                 (cdr element-form)))))
   ((eq (car-safe element-form) '+)
    (make-phony--element-one-or-more
     :element
     (make-phony--element-sequence
      :elements (mapcar
                 (lambda (subelement)
                   (phony--parse-speech-element
                    subelement arglist))
                 (cdr element-form)))))
   ((eq (car-safe element-form) '*)
    (make-phony--element-zero-or-more
     :element
     (make-phony--element-sequence
      :elements (mapcar
                 (lambda (subelement)
                   (phony--parse-speech-element
                    subelement arglist))
                 (cdr element-form)))))
   ((eq (car-safe element-form) 'seq)
    (make-phony--element-sequence
     :elements
     (mapcar
      (lambda (subelement)
        (phony--parse-speech-element
         subelement arglist))
      (cdr element-form))))
   (t (phony--parse-speech-value-element element-form))))

(provide 'phony-element-syntax)
;;; phony-element-syntax.el ends here
