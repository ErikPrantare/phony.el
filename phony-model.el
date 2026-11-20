;;; phony-model.el --- Core model for phony -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
;; Created: 20 Oct 2026

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

;; This file defines structs for grammars, modules and rules.  The
;; structs for patterns are also included from phony-pattern.  There
;; are also some helper functions and macros here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'phony-pattern)
(require 'phony-ag)

(cl-defstruct (phony--grammar
               (:constructor phony--make-grammar)
               (:copier nil))
  "A collection of rules."
  ( rules (make-hash-table)
    :type hash-table
    :documentation "Hash table of rules, indexed by name.")
  ( modules (make-hash-table)
    :type hash-table
    :documentation "Hash table of modules, indexed by name.")
  ( module-provision (make-hash-table)
    :type hash-table
    :documentation "Hash table mapping which modules resolve interfaces where.
The key module will have its interfaces resolved in the module it maps
to."))

(defvar phony--default-grammar (phony--make-grammar)
  "The current user specification of the grammar.")

(defvar phony--exported-grammar (phony--make-grammar)
  "The most recently exported grammar.

This is `phony--default-grammar' after going through preparation.  The
grammar object should never be mutated.")

(defun phony--copy-grammar (grammar)
  "Return a deep copy of GRAMMAR."
  (let ((rules (make-hash-table))
        (modules (make-hash-table)))
    (maphash
     (lambda (name rule)
       (puthash name (phony--copy-rule rule) rules))
     (phony--grammar-rules grammar))
    (maphash
     (lambda (name module)
       (puthash name (phony--copy-module module) modules))
     (phony--grammar-modules grammar))

    (phony--make-grammar
     :rules rules
     :modules modules
     :module-provision
     (copy-hash-table
      (phony--grammar-module-provision grammar)))))

(defun phony--get-rules (grammar)
  "Return a list containing the rules of GRAMMAR."
  (hash-table-values (phony--grammar-rules grammar)))

(defun phony--get-rule (name grammar)
  "Return the rule named NAME in GRAMMAR.
Return nil if no such rule exists."
  (gethash name (phony--grammar-rules grammar)))

(cl-defstruct (phony--module
               (:constructor phony--make-module)
               (:copier phony--copy-module))
  "Modules group rules into a unit.
See `phony-define-module' for information on modules."
  ( name nil
    :type symbol
    :documentation
    "Name of this module.")
  ( utterance nil
    :type string
    :documentation
    "Utterance for referring to this module.")
  ( file-name nil
    :type string
    :documentation
    "Filename of the file defining this module.
If nil it is not associated to a particular file.")
  ( enabled-p nil
    ;; TODO: This should really be called sealed, not disabled.  The
    ;; rules still exist.
    :type boolean
    :documentation
    "Whether the rules of this module should impact the utterance grammar.
See `phony-enable-module' for information on module enablement."))

(cl-defstruct (phony--rule
               (:constructor nil)
               (:copier nil))
  "A rule for matching an utterance."
  ( name nil
    :type symbol
    :documentation "The name of this rule.")
  ( documentation nil
    :type string
    :documentation "The documentation string for this rule.")
  ( module nil
    :type symbol
    :documentation "Name of the module this rule belongs to.
Modules are specified with `phony-define-module' or `phony-module'.  Its
value is nil if it is not part of a module.")
  ( file-name nil
    :type string
    :documentation "The filename of the file where this rule was defined.
Its value is nil if it was not evaluated from a file.")
  ( modes '(global)
    :type list
    :documentation "A list of modes under which this rule is active.
If none of the modes are active, this rule cannot be matched.  A
special case is the \\='global symbol, which indicates that the rule
should always be active.")
  ( contributes-to '()
    :type list
    :documentation "A list of open rules that this rule contributes to.
See documentation for `phony-define-open-rule'.")
  ( provides nil
    :type symbol
    :documentation "An interface this rule is a provider of.
See documentation for `phony-define-interface'.")
  ( virtual-p nil
    :type boolean
    :documentation "Whether this rule was generated by a compilation pass."))

(cl-defstruct (phony--procedure-rule
               (:include phony--rule)
               (:constructor phony--make-procedure-rule))
  "A rule mapping an utterance pattern to a function call."
  ( function '()
    :type function
    :documentation "Function to call when the utterance is matched.")
  ( pattern nil
    :type phony-pattern--sequence
    :documentation "Parsed pattern tree for the utterance pattern.")
  ( arglist '()
    :type (repeat symbol)
    :documentation "List of argument symbols extracted from the pattern.")
  ( interactive-p t
    :type boolean
    :documentation "\
Whether this rule can be invoked directly.
Non-interactive rules are only reachable as sub-rules within other rules.")
  ( anchor-beginning-p nil
    :type boolean
    :documentation "\
Constrain matching to the start of an utterance, like `^' in regex.")
  ( anchor-end-p nil
    :type boolean
    :documentation "\
Constrain matching to the end of an utterance, like `$' in regex."))

(cl-defstruct (phony--open-rule
               (:include phony--rule)
               (:constructor phony--make-open-rule))
  "A rule that matches any of its alternatives."
  ( alternatives nil
    :type (repeat symbol)
    :documentation "List of symbols naming the alternative rules."))

(cl-defstruct (phony--dictionary-storage
               (:constructor phony--make-dictionary-storage)
               (:copier nil))
  "The actual mapping for a `phony--dictionary'.
To have runtime modification of dictionaries work, the exported grammar
needs to share storage for dictionaries with the front-facing
`phony--default-grammar'.  This indirection is for that."
  ( mapping nil
    :type list
    :documentation "An alist mapping utterances, strings, to values.")
  ( generation 0
    :type integer
    :documentation "Integer counting how many times the storage has been updated."))

(cl-defstruct (phony--dictionary
               (:include phony--rule)
               (:constructor nil)
               (:constructor phony--make-dictionary))
  "A dictionary mapping utterances to values."
  ( storage (phony--make-dictionary-storage)
    :type phony--dictionary-storage
    :documentation "The storage for the dictionary alist.
This layer of indirection exists so that runtime changes to the
dictionary are reflected to the corresponding dictionaries in the
exported grammar."))

(defun phony--dictionary-mapping (dictionary)
  "Return the underlying alist of DICTIONARY."
  (phony--dictionary-storage-mapping
   (phony--dictionary-storage dictionary)))

(defun phony--dictionary-generation (dictionary)
  "Return the generation of DICTIONARY."
  (phony--dictionary-storage-generation
   (phony--dictionary-storage dictionary)))

(defun phony--dictionary-touch (dictionary)
  "Mark DICTIONARY as updated.

Do not call this directly.  Use `phony--dictionary-mapping' or
`phony--dictionary-elt' generalized variables for changing the content,
or `phony--dictionary-delete' for deletion."
  (let ((storage (phony--dictionary-storage dictionary)))
    (cl-incf (phony--dictionary-storage-generation storage))))

(gv-define-setter phony--dictionary-mapping (value dictionary)
  (macroexp-let2* nil (dictionary
                       (storage `(phony--dictionary-storage ,dictionary))
                       value)
    `(progn
       (setf (phony--dictionary-storage-mapping ,storage) ,value)
       (phony--dictionary-touch ,dictionary)
       ,value)))

;; We could make dictionaries implement the full map.el interface if
;; we end up adding more functions.
(defun phony--dictionary-elt (dictionary utterance)
  "Return the value of DICTIONARY at UTTERANCE."
  (alist-get
   utterance
   (phony--dictionary-mapping dictionary)
   nil nil #'equal))

(gv-define-setter phony--dictionary-elt (value dictionary utterance)
  (macroexp-let2* nil (dictionary
                       (storage `(phony--dictionary-storage ,dictionary))
                       value utterance)
    `(progn
       (setf (alist-get ,utterance
                        (phony--dictionary-storage-mapping ,storage)
                        nil nil #'equal)
             ,value)
       (phony--dictionary-touch ,dictionary)
       ,value)))

(defun phony--dictionary-delete (dictionary utterance)
  "Remove entry keyed by UTTERANCE from DICTIONARY."
  (let ((storage (phony--dictionary-storage dictionary)))
    (setf (alist-get utterance
                     (phony--dictionary-storage-mapping storage)
                     nil 'remove #'equal)
          nil)
    (phony--dictionary-touch dictionary))
  ;; Mirrors behaviour of `map-delete'
  dictionary)

(cl-defstruct (phony--interface-rule
               (:include phony--rule)
               (:constructor phony--make-interface-rule))
  "An interface rule to be replaced by a provider.

See documentation for `phony-define-interface'."
  ( default nil
    :type symbol
    :documentation
    "The default provider for this interface.
If this is nil, there is no default provider."))

(defun phony--copy-rule-base-inplace (source target)
  "Copy base `phony--rule' slots from SOURCE into TARGET."
  (setf (phony--rule-name target) (phony--rule-name source))
  (setf (phony--rule-documentation target) (phony--rule-documentation source))
  (setf (phony--rule-module target) (phony--rule-module source))
  (setf (phony--rule-file-name target) (phony--rule-file-name source))
  (setf (phony--rule-modes target)
        (copy-sequence (phony--rule-modes source)))
  (setf (phony--rule-contributes-to target)
        (copy-sequence (phony--rule-contributes-to source)))
  (setf (phony--rule-virtual-p target) (phony--rule-virtual-p source))
  (setf (phony--rule-provides target) (phony--rule-provides source))
  target)

(cl-defgeneric phony--copy-rule (rule)
  "Return a deep copy of RULE.")

(cl-defmethod phony--copy-rule ((rule phony--procedure-rule))
  "Return a deep copy of RULE."
  (let ((copy (phony--make-procedure-rule
               :function (phony--procedure-rule-function rule)
               :pattern (phony-pattern--copy (phony--procedure-rule-pattern rule))
               :arglist (copy-sequence (phony--procedure-rule-arglist rule))
               :interactive-p (phony--procedure-rule-interactive-p rule)
               :anchor-beginning-p (phony--procedure-rule-anchor-beginning-p rule)
               :anchor-end-p (phony--procedure-rule-anchor-end-p rule))))
    (phony--copy-rule-base-inplace rule copy)))

(cl-defmethod phony--copy-rule ((rule phony--open-rule))
  "Return a deep copy of RULE."
  (let ((copy (phony--make-open-rule
               :alternatives (copy-sequence
                              (phony--open-rule-alternatives rule)))))
    (phony--copy-rule-base-inplace rule copy)))

(cl-defmethod phony--copy-rule ((rule phony--dictionary))
  "Return a deep copy of RULE.

The copy will share a reference to the same mapping as RULE, so that
runtime modifications to the mapping are reflected in the exported
grammar."
  (let ((copy (phony--make-dictionary
               :storage (phony--dictionary-storage rule))))
    (phony--copy-rule-base-inplace rule copy)))

(cl-defmethod phony--copy-rule ((rule phony--interface-rule))
  "Return a deep copy of RULE."
  (let ((copy (phony--make-interface-rule
               :default (phony--interface-rule-default rule))))
    (phony--copy-rule-base-inplace rule copy)))

(defun phony--rule-active-p (rule)
  "Return non-nil if RULE is active.

A rule is inactive if none of its modes are currently active."
  (let ((modes (phony--rule-modes rule)))
    (or
     (memq 'global modes)
     (derived-mode-p modes)
     (seq-some (lambda (mode)
                 (or
                  (memq mode local-minor-modes)
                  (memq mode global-minor-modes)))
               modes))))

(cl-defmethod phony-ag--children ((grammar phony--grammar))
  "Return the children of GRAMMAR."
  (let ((children (cl-call-next-method)))
    (maphash (lambda (name rule)
               (push (cons (cons 'rule name) rule) children))
             (phony--grammar-rules grammar))
    (maphash (lambda (name rule)
               (push (cons (cons 'module name) rule) children))
             (phony--grammar-modules grammar))
    children))

(cl-defmethod phony-ag--children ((_dictionary phony--dictionary))
  "Return the children of DICTIONARY.

The underlying storage is excluded, as it is shared between different
grammars."
  (let ((children (cl-call-next-method)))
    (setf (alist-get 'storage children nil t) nil)
    children))

(cl-defmethod phony-ag--children ((pattern phony-pattern--sequence))
  "Return the children of PATTERN."
  (seq-map-indexed
   (lambda (child index) (cons index child))
   (phony-pattern--sequence-patterns pattern)))

(cl-defmethod phony-ag--children ((pattern phony-pattern--alternative))
  "Return the children of PATTERN."
  (seq-map-indexed
   (lambda (child index) (cons index child))
   (phony-pattern--alternative-patterns pattern)))

(phony-ag--define-inherited phony--containing-rule (node)
  "Return the rule containing NODE.")

(phony-ag--define-equation phony--containing-rule
    ((rule phony--rule) _)
  rule)

(phony-ag--define-inherited phony--containing-grammar (node)
  "Return the grammar containing NODE.")

(phony-ag--define-equation phony--containing-grammar
    ((grammar phony--grammar) _)
  grammar)

(phony-ag--define-inherited phony--containing-module (node)
  "Return the module containing NODE.

If NODE does not belong to a module, this function returns nil.")

(phony-ag--define-equation phony--containing-module ((grammar phony--grammar)
                                                     (rule-slot (head rule)))
  (and-let* ((rule-name (cdr rule-slot))
             (rule (phony--get-rule rule-name grammar))
             (module-name (phony--rule-module rule))
             (module (gethash module-name (phony--grammar-modules grammar))))
    module))

(provide 'phony-model)

;;; phony-model.el ends here
