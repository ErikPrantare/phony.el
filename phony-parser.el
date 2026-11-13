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
(require 'seq)

(require 'phony-model)
(require 'phony-ag)

(cl-defstruct (phony-parser--parser
               (:constructor phony-parser--make)
               (:copier nil))
  "Step-based parser over token sequences."
  parses-function step-function parses-cache
  ;; Fresh cons is always unequal to the actual generation.
  (generation (cons nil nil)))

(defvar phony-parser--current-generation -1
  "The current generation of parsing.  This is incremented each parse.")

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
  (unless (eql phony-parser--current-generation
               (phony-parser--parser-generation parser))
    (setf (phony-parser--parser-parses-cache parser)
          (funcall (phony-parser--parser-parses-function parser)))
    (setf (phony-parser--parser-generation parser)
          phony-parser--current-generation))
  (phony-parser--parser-parses-cache parser))

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

(defun phony-parser--runtime-resolved-cached (parser-factory generation-function)
  "Return a parser behaving like PARSER-FACTORY invoked at runtime.

The results are cached.  The cache is invalidated whenever the return
value of GENERATION-FUNCTION changes.

PARSER-FACTORY is invoked with no arguments to yield the parser to use."
  (let ((generation (cons nil nil)) ; Fresh cons is never eql to value
                                    ; of GENERATION-FUNCTION.
        (parser nil))
    (phony-parser--runtime-resolved
     (lambda ()
       (let ((current-generation (funcall generation-function)))
         (unless (eql current-generation generation)
           (setq parser (funcall parser-factory))
           (setq generation current-generation)))
       parser))))

(defun phony-parser--rule-active-guard (rule)
  "Return a parser that succeeds if RULE is currently active."
  (phony-parser--guard (lambda () (phony--rule-active-p rule))))

(defvar phony-parser--literal-normalizer #'identity
  "Function for normalizing tokens and literals.

Comparison is done on the normalized strings.")

(defun phony-parser--literal (string)
  "Return a parser that accepts tokens matching STRING.

STRING is split up into words.  The returned parser reads that many
tokens before accepting, if the tokens correspond to the words.
The result of a parse is STRING.

Uses `phony-parser--literal-normalizer' to normalize tokens and
literals."
  (let ((words (string-split string)))
    (apply
     #'phony-parser--lift
     (lambda (&rest _words) string)
     (seq-map (lambda (word)
                (phony-parser--satisfy
                 (lambda (token)
                   (equal
                    (funcall phony-parser--literal-normalizer word)
                    (funcall phony-parser--literal-normalizer token)))))
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

(cl-defgeneric phony-parser--collect-arguments-from-tree (match-tree pattern arglist)
  "Extract argument bindings from MATCH-TREE according to PATTERN and ARGLIST.")

(cl-defmethod phony-parser--collect-arguments-from-tree (_match-tree (_pattern phony--pattern-literal) arglist)
  ;; checkdoc-params: (arglist)
  "Collect arguments from LITERAL."
  (make-list (seq-length arglist) nil))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (pattern phony--pattern-sequence) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from SEQUENCE.

If an argument occurs in multiple children of PATTERN, an
unspecified alternative is picked."
  (apply #'seq-mapn (lambda (&rest patterns) (seq-some #'identity patterns))
         (seq-mapn (lambda (match-value pattern)
                     (phony-parser--collect-arguments-from-tree match-value pattern arglist))
                   match-tree (phony--pattern-sequence-patterns pattern))))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (optional phony--pattern-optional) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from OPTIONAL."
  (if match-tree
      (phony-parser--collect-arguments-from-tree
       match-tree
       (phony--pattern-optional-pattern optional)
       arglist)
    (make-list (seq-length arglist) nil)))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (pattern phony--pattern-argument) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from argument PATTERN."
  (seq-map (lambda (argument)
             (and (eq argument (phony--pattern-argument-name pattern))
                  match-tree))
           arglist))

(cl-defmethod phony-parser--collect-arguments-from-tree (match-tree (pattern phony--pattern-repeat) arglist)
  ;; checkdoc-params: (match-tree arglist)
  "Collect arguments from REPEAT.

Arguments occurring within PATTERN are collected into list forms."
  (if (null match-tree)
      (make-list (seq-length arglist) nil)
    (let ((occurring-arguments
           (seq-map #'phony--pattern-argument-name
                    (phony--pattern-collect #'phony--pattern-argument-p pattern))))
      (seq-mapn (lambda (match-data argument)
                  (and (seq-contains-p occurring-arguments argument)
                       `(list ,@match-data)))
                (apply #'seq-mapn #'list
                       (seq-map (lambda (match-value)
                                  (phony-parser--collect-arguments-from-tree
                                   match-value
                                   (phony--pattern-repeat-pattern pattern)
                                   arglist))
                                match-tree))
                arglist))))

(phony-ag--define-synthesized phony-parser--parser (node)
  "Return a parser matching NODE.

For an pattern, the parser produces a match tree.  For a rule, it
produces an AST.  For a grammar, it produces a list of ASTs, one for
each command in the uttered sequence.")

(cl-defmethod phony-parser--parser ((literal phony--pattern-literal))
  "Return a parser matching LITERAL."
  (phony-parser--literal (phony--pattern-literal-string literal)))

(cl-defmethod phony-parser--parser ((sequence phony--pattern-sequence))
  "Return a parser matching each pattern of SEQUENCE in order."
  (apply
   #'phony-parser--lift #'list
   (seq-map #'phony-parser--parser
            (phony--pattern-sequence-patterns sequence))))

(cl-defmethod phony-parser--parser ((optional phony--pattern-optional))
  "Return a parser matching OPTIONAL zero or one times."
  (phony-parser--optional
   (phony-parser--parser (phony--pattern-optional-pattern optional))))

(cl-defmethod phony-parser--parser ((repeat phony--pattern-zero-or-more))
  "Return a parser matching REPEAT zero or more times."
  (phony-parser--zero-or-more
   (phony-parser--parser (phony--pattern-zero-or-more-pattern repeat))))

(cl-defmethod phony-parser--parser ((repeat phony--pattern-one-or-more))
  "Return a parser matching REPEAT one or more times."
  (phony-parser--one-or-more
   (phony-parser--parser (phony--pattern-one-or-more-pattern repeat))))

(cl-defmethod phony-parser--parser ((argument phony--pattern-argument))
  "Return a parser matching the inner pattern of ARGUMENT."
  (phony-parser--parser (phony--pattern-argument-pattern argument)))

(cl-defmethod phony-parser--parser ((_ phony--pattern-external-rule))
  "Return the empty parser.

External rules cannot be parsed locally."
  (phony-parser--empty))

(cl-defmethod phony-parser--parser ((rule phony--pattern-rule))
  "Return a parser matching the rule named by RULE."
  (let ((grammar (phony--containing-grammar rule))
        (name (phony--pattern-rule-name rule)))
    (phony-parser--parser (phony--get-rule name grammar))))

(cl-defmethod phony-parser--parser ((_ phony--pattern-builtin-word))
  "Return parser accepting an arbitrary word."
  (phony-parser--any))

(cl-defmethod phony-parser--parser ((_ phony--pattern-builtin-phrase))
  "Return parser accepting an arbitrary sequence of words.

The result is returned as one string, each word interspersed with a
space."
  (phony-parser--map
   (lambda (strings) (string-join strings " "))
   (phony-parser--one-or-more (phony-parser--any))))

(cl-defmethod phony-parser--parser ((rule phony--procedure-rule))
  "Return a parser matching RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--map
    (lambda (match-tree)
      `(,(phony--rule-name rule)
        ,@(phony-parser--collect-arguments-from-tree
           match-tree
           (phony--procedure-rule-pattern rule)
           (phony--procedure-rule-arglist rule))))
    (phony-parser--parser (phony--procedure-rule-pattern rule)))))

(defun phony-parser--trie-parser (entries)
  "Make a parser for ENTRIES.

ENTRIES is a list, each element of the form (LITERALS . VALUE).  This
function returns a parser that matches a sequence of LITERALS, producing
VALUE.

The parser is constructed using a trie, so that parsing is linear with
respect to the length of LITERALS, and constant with respect to the
length of ENTRIES."
  ;; NOTE: Potential blowup if a ENTRIES contains a large amount of
  ;; long common prefixes.
  (let ((child-parsers (make-hash-table :test 'equal))
        (values nil))
    (pcase-dolist (`(,literals . ,value) entries)
      (if (not literals)
          (push value values)
        (puthash (car literals)
                 (phony-parser--trie-parser
                  (cl-loop for entry in entries
                           when (equal (car literals) (caar entry))
                           collect (cons (cdar entry) (cdr entry))))
                 child-parsers)))
    (phony-parser--make
     :parses-function (lambda () values)
     :step-function
     (lambda (item)
       (gethash (funcall phony-parser--literal-normalizer item)
                child-parsers
                (phony-parser--empty))))))

(defun phony-parser--make-dictionary-parser (dictionary)
  "Return parser unconditionally matching an entry of DICTIONARY."
  (phony-parser--trie-parser
   (mapcar (pcase-lambda (`(,utterance . _))
             (cons (seq-map phony-parser--literal-normalizer
                            (string-split utterance))
                   utterance))
           (phony--dictionary-mapping dictionary))))

(cl-defmethod phony-parser--parser ((rule phony--dictionary))
  "Return a parser matching any entry of dictionary RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--map
    (lambda (utterance) `(,(phony--dictionary-name rule) ,utterance))
    (phony-parser--runtime-resolved-cached
     (lambda () (phony-parser--make-dictionary-parser rule))
     (lambda () (phony--dictionary-generation rule))))))

(cl-defmethod phony-parser--parser ((rule phony--open-rule))
  "Return a parser matching any alternative of open RULE."
  (phony-parser--then
   (phony-parser--rule-active-guard rule)
   (phony-parser--alternative
    (seq-map #'phony-parser--parser
             (phony--resolved-alternatives rule)))))

(cl-defmethod phony-parser--parser ((grammar phony--grammar))
  "Return parser matching active interactive procedure rules of GRAMMAR."
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
                 (seq-map #'phony-parser--parser rules))))
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
           (seq-map #'phony-parser--parser no-anchor)))
         (phony-parser--optional
          (phony-parser--map
           #'list
           (from-alternatives last-anchor)))))))))

(defun phony-parser--parse-utterance (utterance &optional grammar)
  "Parse UTTERANCE string into a list of possible parse results.

Rules are taken from GRAMMAR or `phony--exported-grammar' if it is nil
or not given.  GRAMMAR must be a prepared grammar."
  (setq grammar (or grammar phony--exported-grammar))
  (cl-incf phony-parser--current-generation)
  (phony-parser--parse
   (phony-parser--parser grammar)
   (string-split utterance)))

(provide 'phony-parser)
;;; phony-parser.el ends here
