;;; phony-parser.el --- Combinator parser for phony grammars -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare
;; Created: 24 Jun 2026

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

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'phony-model)
(require 'seq)

(cl-defstruct (phony-parser--parser
               (:constructor phony-parser--make)
               (:copier nil))
  "Step-based parser over token sequences."
  parses-function step-function)

(defvar phony-parser--empty-parser
  (phony-parser--make
   :parses-function (lambda () nil)
   :step-function (lambda (_item) phony-parser--empty-parser))
  "Parser that produces no results for any input.")

(defun phony-parser--empty-p (parser)
  "Return non-nil if PARSER is the empty parser."
  (eq parser phony-parser--empty-parser))

(defun phony-parser--parses (parser)
  "Return the completed results of PARSER."
  (funcall (phony-parser--parser-parses-function parser)))

(defun phony-parser--step (parser item)
  "Advance PARSER by one token ITEM and return the resulting parser."
  (funcall (phony-parser--parser-step-function parser) item))

(defun phony-parser--input-step (input)
  "Return the next item and remaining input from INPUT, or nil."
  (and input (cons (car input) (cdr input))))

(defun phony-parser--run (parser input)
  "Return all completed results of running PARSER on INPUT."
  (cl-loop
   for step = (phony-parser--input-step input)
   while step
   do (pcase-let* ((`(,item . ,tail) step))
        (setq parser (phony-parser--step parser item))
        (setq input tail))
   finally return (phony-parser--parses parser)))

(defun phony-parser--any ()
  "Return a parser that accepts any single token."
  (phony-parser--make
   :parses-function (lambda () nil)
   :step-function
   (lambda (item)
     (phony-parser--return item))))

(defun phony-parser--join (parser)
  "Flatten PARSER.

Each result of PARSER is itself a parser.  These inner parsers
are run on subsequent tokens and their results collected."
  (if (phony-parser--empty-p parser)
      (phony-parser--empty)
    (phony-parser--make
     :parses-function
     (lambda ()
       (seq-mapcat #'phony-parser--parses (phony-parser--parses parser)))
     :step-function
     (lambda (item)
       (phony-parser--alternative
        (cons
         (phony-parser--join (phony-parser--step parser item))
         (seq-map (lambda (parse)
                    (phony-parser--step parse item))
                  (phony-parser--parses parser))))))))

(defun phony-parser--map (f parser)
  "Return a parser applying F to each result of PARSER."
  (if (phony-parser--empty-p parser)
      (phony-parser--empty)
    (phony-parser--make
     :parses-function
     (lambda ()
       (seq-map f (phony-parser--parses parser)))
     :step-function
     (lambda (item)
       (phony-parser--map f (phony-parser--step parser item))))))

(defun phony-parser--bind (parser f)
  "Return a parser that feeds each result of PARSER into F and flattens."
  (phony-parser--join (phony-parser--map f parser)))

(defun phony-parser--return (x)
  "Return a parser that succeeds immediately with X."
  (phony-parser--make
   :parses-function (lambda () (list x))
   :step-function (lambda (_item) (phony-parser--empty))))

(defun phony-parser--empty ()
  "Return the empty parser."
  phony-parser--empty-parser)

(defun phony-parser--alternative (parsers)
  "Return a parser that tries each of PARSERS and collects all results."
  (let ((parsers (cl-remove-if #'phony-parser--empty-p parsers)))
    (cond
     ((null parsers)
      (phony-parser--empty))
     ((null (cdr parsers))
      (car parsers))
     (t
      (phony-parser--make
       :parses-function
       (lambda ()
         (seq-mapcat #'phony-parser--parses parsers))
       :step-function (lambda (item)
                        (phony-parser--alternative
                         (seq-map
                          (lambda (parser) (phony-parser--step parser item))
                          parsers))))))))

(defun phony-parser--satisfy (f)
  "Return a parser that accepts a token satisfying F."
  (phony-parser--make
   :parses-function (lambda () nil)
   :step-function
   (lambda (token)
     (if (funcall f token)
         (phony-parser--return token)
       (phony-parser--empty)))))

(defun phony-parser--then (parser1 parser2)
  "Return a parser that sequences PARSER1 then PARSER2, keeping PARSER2 results."
  (phony-parser--bind parser1 (lambda (_) parser2)))

(defun phony-parser--lift (f &rest parsers)
  "Return a parser applying F to the combined results of PARSERS."
  (if parsers
      (phony-parser--bind
       (car parsers)
       (lambda (x)
         (apply #'phony-parser--lift
                (lambda (&rest args) (apply f x args))
                (cdr parsers))))
    (phony-parser--return (funcall f))))

(defun phony-parser--parse (parser input)
  "Return all completed results of running PARSER on INPUT."
  (phony-parser--run parser input))

(defun phony-parser--guard (pred)
  "Return a parser that succeeds if PRED returns non-nil."
  (phony-parser--bind
   (phony-parser--return nil)
   (lambda (_)
     (if (funcall pred)
         (phony-parser--return nil)
       (phony-parser--empty)))))

(defun phony-parser--runtime-resolved (parser-factory)
  "Return a parser behaving like PARSER-FACTORY invoked at runtime.

Whenever the returned parser is used, PARSER-FACTORY is evaluated with
no arguments to yield the actual parser to use."
  (phony-parser--make
   :parses-function
   (lambda () (phony-parser--parses (funcall parser-factory)))
   :step-function
   (lambda (item) (phony-parser--step (funcall parser-factory) item))))

(defun phony-parser--rule-active-guard (rule)
  "Return a parser that succeeds if RULE is currently active."
  (phony-parser--guard (lambda () (phony--rule-active-p rule))))

(defvar phony-parser--literal-comparator 'string-equal
  "Comparator used for comparing tokens with literal words.
Called with the token as the first paramater, and the literal word as
the second parameter.")

(defun phony-parser--literal (string)
  "Return a parser that accepts tokens matching STRING.

STRING is split up into words.  The returned parser reads that many
tokens before accepting, if the tokens correspond to the words.
The result of a parse is STRING.

Uses `phony-parser--literal-comparator' for the comparison between token
and literal."
  (let ((words (string-split string)))
    (apply
     #'phony-parser--lift
     (lambda (&rest _words) string)
     (seq-map (lambda (word)
                (phony-parser--satisfy
                 (apply-partially phony-parser--literal-comparator word)))
              words))))

(defun phony-parser--optional (parser)
  "Return a parser that matches PARSER zero or one times."
  (phony-parser--alternative
   (list parser
         (phony-parser--return nil))))

(defun phony-parser--zero-or-more (parser)
  "Return a parser that matches PARSER zero or more times."
  (phony-parser--optional (phony-parser--one-or-more parser)))

(defun phony-parser--one-or-more (parser)
  "Return a parser that matches PARSER one or more times."
  (phony-parser--bind
   parser
   (lambda (parse)
     (phony-parser--map
      (lambda (parse-tail) (cons parse parse-tail))
      (phony-parser--zero-or-more parser)))))

(cl-defgeneric phony-parser--collect-arguments-from-tree (match-tree element arglist)
  "Extract argument bindings from MATCH-TREE according to ELEMENT and ARGLIST.")

(cl-defmethod phony-parser--collect-arguments-from-tree (_match-tree (_element phony--element-literal) arglist)
  ;; checkdoc-params: (arglist)
  "Collect arguments from LITERAL."
  (make-list (seq-length arglist) nil))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (element phony--element-sequence) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from SEQUENCE.

If an argument occurs in multiple children of ELEMENT, an
unspecified alternative is picked."
  (apply #'seq-mapn (lambda (&rest elements) (seq-some #'identity elements))
         (seq-mapn (lambda (match-value element)
                     (phony-parser--collect-arguments-from-tree match-value element arglist))
                   match-tree (phony--element-sequence-elements element))))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (optional phony--element-optional) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from OPTIONAL."
  (if match-tree
      (phony-parser--collect-arguments-from-tree
       match-tree
       (phony--element-optional-element optional)
       arglist)
    (make-list (seq-length arglist) nil)))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (element phony--element-argument) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from argument ELEMENT."
  (seq-map (lambda (argument)
             (and (eq argument (phony--element-argument-name element))
                  match-tree))
           arglist))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (element phony--element-repeat) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from REPEAT.

Arguments occurring within ELEMENT are collected into list forms."
  (if (null match-tree)
      (make-list (seq-length arglist) nil)
    (let ((occurring-arguments
           (seq-map #'phony--element-argument-name
                    (phony--element-collect #'phony--element-argument-p element))))
      (seq-mapn (lambda (match-data argument)
                  (and (seq-contains-p occurring-arguments argument)
                       `(list ,@match-data)))
                (apply #'seq-mapn #'list
                       (seq-map (lambda (match-value)
                                  (phony-parser--collect-arguments-from-tree
                                   match-value
                                   (phony--element-repeat-element element)
                                   arglist))
                                match-tree))
                arglist))))

(phony--define-grammar-attribute phony-parser--rule-cache (grammar)
  "Return hash table mapping rule names to their parsers in GRAMMAR."
  (make-hash-table))
(declare-function phony-parser--rule-cache nil)

(cl-defgeneric phony-parser--from-element (element grammar)
  "Build a parser from ELEMENT.

References to other rules are resolved through GRAMMAR.")

(cl-defmethod phony-parser--from-element ((literal phony--element-literal) _grammar)
  "Return a parser matching LITERAL."
  (phony-parser--literal (phony--element-literal-string literal)))

(cl-defmethod phony-parser--from-element ((sequence phony--element-sequence) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching each element of SEQUENCE in order."
  (apply
   #'phony-parser--lift #'list
   (seq-map (lambda (element) (phony-parser--from-element element grammar))
            (phony--element-sequence-elements sequence))))

(cl-defmethod phony-parser--from-element ((optional phony--element-optional) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching OPTIONAL zero or one times."
  (phony-parser--optional
   (phony-parser--from-element (phony--element-optional-element optional) grammar)))

(cl-defmethod phony-parser--from-element ((repeat phony--element-zero-or-more) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching REPEAT zero or more times."
  (phony-parser--zero-or-more
   (phony-parser--from-element (phony--element-zero-or-more-element repeat) grammar)))

(cl-defmethod phony-parser--from-element ((repeat phony--element-one-or-more) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching REPEAT one or more times."
  (phony-parser--one-or-more
   (phony-parser--from-element (phony--element-one-or-more-element repeat) grammar)))

(cl-defmethod phony-parser--from-element ((argument phony--element-argument) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching the inner element of ARGUMENT."
  (phony-parser--from-element (phony--element-argument-element argument) grammar))

(cl-defmethod phony-parser--from-element ((_ phony--element-external-rule) _grammar)
  "Return the empty parser.

External rules cannot be parsed locally."
  (phony-parser--empty))

(cl-defmethod phony-parser--from-element ((rule phony--element-rule) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching the rule named by RULE."
  (let ((name (phony--element-rule-name rule)))
    (with-memoization (gethash name (phony-parser--rule-cache grammar))
      (phony-parser--from-rule (phony--get-rule name grammar) grammar))))

(cl-defmethod phony-parser--from-element ((_ phony--element-builtin-word) _grammar)
  "Return parser accepting an arbitrary word."
  (phony-parser--any))

(cl-defmethod phony-parser--from-element ((_ phony--element-builtin-phrase) _grammar)
  "Return parser accepting an arbitrary sequence of words.

The result is returned as one string, each word interspersed with a
space."
  (phony-parser--map
   (lambda (strings) (string-join strings " "))
   (phony-parser--one-or-more (phony-parser--any))))

(cl-defgeneric phony-parser--from-rule (rule grammar)
  "Return a parser matching RULE and producing an AST.

References to other rules are resolved through GRAMMAR.")

(cl-defmethod phony-parser--from-rule ((rule phony--procedure-rule) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--map
    (lambda (match-tree)
      `(,(phony--rule-name rule)
        ,@(phony-parser--collect-arguments-from-tree
           match-tree
           (phony--procedure-rule-element rule)
           (phony--procedure-rule-arglist rule))))
    (phony-parser--from-element (phony--procedure-rule-element rule) grammar))))

(cl-defmethod phony-parser--from-rule ((rule phony--dictionary) _grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching any entry of dictionary RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--map
    (lambda (utterance) `(,(phony--dictionary-name rule) ,utterance))
    (phony-parser--runtime-resolved
     (lambda ()
       (phony-parser--alternative
        (seq-map #'phony-parser--literal
                 (map-keys (phony--dictionary-mapping rule)))))))))

(cl-defmethod phony-parser--from-rule ((rule phony--open-rule) grammar)
  ;; checkdoc-params: (grammar)
  "Return a parser matching any alternative of open RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--alternative
    (seq-map (lambda (production) (phony-parser--from-rule
                                   (phony--get-rule production grammar)
                                   grammar))
             (phony--open-rule-alternatives rule)))))

(defun phony-parser--make-parser (grammar)
  "Build a parser matching all active interactive procedure rules of GRAMMAR."
  (let* ((rules (seq-filter
                 (lambda (rule)
                   (and (phony--procedure-rule-p rule)
                        (phony--procedure-rule-interactive-p rule)))
                 (phony--get-rules grammar)))
         (anchor-groups
          (seq-group-by
           (lambda (rule)
             (cons (phony--procedure-rule-anchor-beginning-p rule)
                   (phony--procedure-rule-anchor-end-p rule)))
           rules))
         (no-anchor (alist-get '(nil . nil) anchor-groups nil nil #'equal))
         (first-anchor (alist-get '(t . nil) anchor-groups nil nil #'equal))
         (last-anchor (alist-get '(nil . t) anchor-groups nil nil #'equal))
         (lonely-anchor (alist-get '(t . t) anchor-groups nil nil #'equal)))
    (cl-flet ((from-alternatives (rules)
                (phony-parser--alternative
                 (seq-map (lambda (rule)
                            (phony-parser--from-rule rule grammar))
                          rules))))
      (phony-parser--alternative
       (list
        (phony-parser--map
         #'list
         (from-alternatives lonely-anchor))
        (phony-parser--lift
         #'append
         (phony-parser--optional
          (phony-parser--map
           #'list
           (from-alternatives first-anchor)))
         (phony-parser--zero-or-more
          (phony-parser--alternative
           (seq-map (lambda (rule)
                      (phony-parser--from-rule rule grammar))
                    no-anchor)))
         (phony-parser--optional
          (phony-parser--map
           #'list
           (from-alternatives last-anchor)))))))))

(phony--define-grammar-attribute phony-parser--get-parser (grammar)
  "Return a parser for matching GRAMMAR."
  (phony-parser--make-parser grammar))
(declare-function phony-parser--get-parser nil)

(defun phony-parser--parse-utterance (utterance &optional grammar)
  "Parse UTTERANCE string into a list of possible parse results.

Rules are taken from GRAMMAR or `phony--exported-grammar' if it is nil
or not given.  GRAMMAR must be a prepared grammar."
  (phony-parser--parse
   (phony-parser--get-parser (or grammar phony--exported-grammar))
   (string-split utterance)))

(provide 'phony-parser)
;;; phony-parser.el ends here
