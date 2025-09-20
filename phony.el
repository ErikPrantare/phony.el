;;; phony.el --- Speech bindings for Elisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Erik Präntare

;; Author: Erik Präntare
;; Keywords: files
;; Version: 0.1.0
;; Homepage: https://github.com/ErikPrantare/phony.el
;; Package-Requires: ((emacs "28.1"))
;; Created: 13 Jul 2024

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup phony nil
  "Functionality for defining speech bindings."
  :group 'files)

(defcustom phony-output-directory nil
  "Directory for exported rules."
  :type 'directory
  :group 'phony)

(defun phony--to-python-identifier (symbol)
  "Generate a fitting python identifier for SYMBOL.

This function is used to automatically generate external names for
rules."
  (concat "phony_" (replace-regexp-in-string (rx (not alnum)) "_" (symbol-name symbol))))

(cl-defstruct (phony--rule
               (:constructor nil))
  "A rule for matching an utterance.

NAME is the name of the rule.

EXTERNAL-NAME is the name this rule will have for the speech recognition
engine, and should be a string."
  (name nil :type symbol)
  (external-name nil :type string))

(cl-defstruct (phony--procedure-rule
               (:include phony--rule))
  (function '() :type function)
  (elements '() :type (repeat
                       (choice phony--element-literal
                               phony--element-optional
                               phony--element-one-or-more
                               phony--element-zero-or-more
                               phony--element-argument
                               phony--element-external-rule
                               phony--element-rule)))
  (arglist '() :type (repeat symbol))
  (export t :type boolean)
  (modes '(global) :type (repeat symbol))
  (anchor-beginning-p nil :type boolean)
  (anchor-end-p nil :type boolean))

(cl-defstruct (phony--open-rule
               (:include phony--rule))
  (alternatives nil :type (repeat symbol))
  (transformation nil :type function))

(cl-defstruct (phony--dictionary
               (:include phony--procedure-rule)
               (:constructor nil)
               (:constructor phony--make-dictionary
                             (name
                              &key
                              (external-name (phony--to-python-identifier name))
                              format-raw-p)))
  "A dictionary mapping utterances to values.

NAME must be the symbol containing the mapping, an alist.

FORMAT-RAW-P is t if this dictionary is intended to be used on the
speech recognition side.  In that case, the mapping may only map to
strings."
  (format-raw-p nil :type boolean))

(defun phony--dictionary-alist (dictionary)
  "Return the alist stored in DICTIONARY."
  (funcall (phony--dictionary-name dictionary)))

(defun phony--normalize-rule (rule-or-name)
  "Return rule specified by RULE-OR-NAME.
RULE-OR-NAME must be a `phony--rule' or a symbol naming a rule."
  (if (symbolp rule-or-name)
      (phony--get-rule rule-or-name)
    rule-or-name))

(defun phony--external-name (rule-or-name)
  "Return the external name for RULE-OR-NAME.

The external name of a rule is a string, representing how the function
is identified when exported to the speech engine."
  (phony--rule-external-name
   (phony--normalize-rule rule-or-name)))

(defvar phony--rules (make-hash-table)
  "Hash table of all defined rules, indxed by name.")

(defun phony--get-rules ()
  "Return a list containing all defined rules."
  (hash-table-values phony--rules))

(defun phony--get-rule (name)
  "Get the rule named NAME.
Return nil if no such rule exists."
  (gethash name phony--rules))

(defun phony--add-rule (rule)
  "Add a new rule RULE.

This function must be invoked every time a rule is defined."
  (puthash (phony--rule-name rule)
           rule
           phony--rules))

(defun phony-remove-rule (rule-name)
  "Remove rule named RULE-NAME."
  (interactive (list (intern (completing-read "Remove rule: "
                                              (hash-table-keys
                                               phony--rules)))))
  (remhash rule-name phony--rules)
  (seq-doseq (open-rule (seq-filter
                         #'phony--open-rule-p
                         (phony--get-rules)))
    (setf (phony--open-rule-alternatives open-rule)
          (remove rule-name (phony--open-rule-alternatives open-rule))))
  (phony-request-export))

(defun phony-dictionary-get (utterance dictionary)
  "Return the value corresponding to UTTERANCE in DICTIONARY.
If no such value exists, return nil."
  (funcall dictionary utterance))

(defmacro phony-dictionary-put (utterance dictionary value)
  "Set the value of UTTERANCE in DICTIONARY to VALUE.
If value is nil, remove the utterance from the list instead.

Invoking this function will resync the dictionary to the external speech
recognition engine."
  (declare (indent defun))
  `(prog1
       (setf (alist-get ,utterance ,dictionary nil t #'equal) ,value)
     (phony--request-sync-dictionaries)))

(gv-define-expander phony-dictionary-get
  ;; We need to use `gv-define-expander', because the simpler versions
  ;; expand to a let-expression binding the dictionary to a local
  ;; variable.  That meant removing elements became impossible.
  (lambda (do utterance dictionary)
    (funcall do `(phony-dictionary-get ,utterance ,dictionary)
             (lambda (value)
               `(phony-dictionary-put ,utterance ,dictionary ,value)))))

(defun phony--create-lookup-representation (entry dictionary)
  "Create lookup string for ENTRY in DICTIONARY.

When evaluating the returned string from emacsclient, this performs
the lookup."
  (if (phony--dictionary-format-raw-p dictionary)
      (cdr entry)
    (format "(%s \"%s\")"
            (phony--dictionary-name dictionary)
            (car entry))))

(defun phony--prepare-dictionary-for-serialization (dictionary)
  "Return DICTIONARY as an entry for `json-serialize'.

This represents one key-value pair, mapping the external dictionary name
to its dictionary."
  (cons (make-symbol (phony--external-name dictionary))
        (mapcar (lambda (entry)
                  (cons
                   (make-symbol (car entry))
                   ;; Each value is a string, encoding a form that
                   ;; will evaluate to the actual value.
                   (phony--create-lookup-representation
                    entry dictionary)))
                (phony--dictionary-alist dictionary))))

;; TODO: Handle IO errors
(defun phony--send-dictionaries ()
  "Write dictionaries to dictionaries.json.

The file is relative to `phony-output-directory'

The speech recognition backend can read this file to register the
dictionaries."
  (with-temp-file (file-name-concat phony-output-directory
                                    "dictionaries.json")
    (json-insert
     (mapcar #'phony--prepare-dictionary-for-serialization
             (seq-filter #'phony--dictionary-p (phony--get-rules))))))

(defun phony--request-sync-dictionaries ()
  "Sync DICTIONARY-NAMES when next idle."
  (interactive)
  (cancel-function-timers #'phony--send-dictionaries)
  (run-with-idle-timer 0.0 nil #'phony--send-dictionaries))

(cl-defun phony--define-dictionary (name
                                    mapping
                                    &key
                                    (external-name nil)
                                    (format-raw nil))
  "Define dictionary rule NAME containing MAPPING.

If provided, EXTERNAL-NAME specifies the name that the rule will go
under for the external speech engine.  If FORMAT-RAW is t, the
dictionary will be formated for use on the side of the speech
recognition engine.  This only works if the values of the dictionary are
strings."
  (setq external-name (or external-name
                          (phony--to-python-identifier name)))

  (unless (proper-list-p mapping)
    (error "The mapping of %s must be a list" name))

  (when-let ((non-cons (seq-find (lambda (x) (not (consp x))) mapping)))
    (error "The mapping of %s must be a alist, but %S is not a cons cell"
           name non-cons))

  (when-let ((non-string-key (seq-find
                              (lambda (entry) (not (stringp (car-safe entry))))
                              mapping)))
    (error "The keys of %s must be strings, but %S is not a string"
           name (car non-string-key)))

  (defalias name
    (lambda (&optional utterance new-value)
      (:documentation (concat "Return the alist of dictionary `"
                                 (symbol-name name)
                                 "'.\nIf UTTERANCE is given, return instead the corresponding value of the
alist.  If NEW-VALUE is provided as well, associate instead UTTERANCE
to NEW-VALUE in this dictionary."))
      (if utterance
          (if new-value
              (setf (alist-get utterance (symbol-value name) nil nil #'equal)
                    new-value)
            (alist-get utterance (symbol-value name) nil nil #'equal))
        (symbol-value name))))

  (eval `(gv-define-simple-setter ,name ,name))

  (phony--add-rule
   (phony--make-dictionary
    name
    :external-name external-name
    :format-raw-p format-raw))
  (phony--request-sync-dictionaries)

  ;; Needs to return the actual mapping, see `phony-define-dictionary'
  mapping)

(defun phony--split-keywords-rest (declaration-args)
  "Split DECLARATION-ARGS into keyword arguments and rest arguments.

Keyword arguments are assumed to occur first.  Returns a cons-cell with
the car being a plist of the keyword arguments and the cdr the rest
arguments."
  (let ((arguments '()))
    (while (keywordp (car declaration-args))
      (push (seq-take declaration-args 2) arguments)
      (setq declaration-args (seq-drop declaration-args 2)))
    (cons (apply #'append arguments) declaration-args)))

(defmacro phony-define-dictionary (name &rest arguments)
  "Define a dictionary with NAME and containing ALIST.

ALIST is an alist mapping utterances to values.  An utterance is a
string containing the spoken form for referencing the value.  The
defined rule matches on any of the keys and evaluates to the
corresponding value.

Optional arguments are given as named arguments before ALIST.  They can
be one of the following:

  :external-name    Name to expose to the external engine.
  :format-raw       Export values in a form usable by the engine,
                    not Emacs.  This only works when the values are
                    strings.

\(fn NAME [KEY VALUE]... ALIST)"
  (declare (indent defun))
  (let ((split-arguments (phony--split-keywords-rest arguments)))
    ;; We need to expand to defvar, or else xref will not find the
    ;; definition.  defvar only modifies the variable when it is void,
    ;; so if it is not we revert to setq.
    ;; TODO: Remove the variable definition
    `(,(if (boundp name) 'setq 'defvar)
      ,name
      (phony--define-dictionary ',name ,@(cdr split-arguments) ,@(car split-arguments)))))

(defun phony--add-alternative (alternative open-rule-name)
  "Add rule ALTERNATIVE as an alternative for open rule OPEN-RULE-NAME."
  (let ((rule (phony--get-rule open-rule-name)))
    (unless rule
      (error "No rule %s defined" open-rule-name))
    (unless (phony--open-rule-p rule)
      (error "No open rule %s defined" open-rule-name))
    (cl-pushnew alternative (phony--open-rule-alternatives rule))))

(cl-defmacro phony-define-open-rule (name &key
                                          (alternatives nil)
                                          (contributes-to nil)
                                          (transformation nil)
                                          (external-name nil))
  "Define NAME as an open rule.

Open rules match any of the rules specified in ALTERNATIVES.  Other
rules can contribute to the list of alternatives through the
CONTRIBUTES-TO argument CONTRIBUTES-TO is an open rule or list of open
rules that this rule contributes to.  See also `phony-rule', which
admits this argument as well.

If a function TRANSFORMATION is given, the value of the matched
alternative is first passed through TRANSFORMATION to create the value
of matching this rule.  Otherwise, the value is passed through without
modification.

If EXTERNAL-NAME is given, it will be used for the name generated for
this rule in the external speech engine."
  (declare (indent defun))
  `(progn
     (cl-assert (symbolp ,transformation) nil
                "Argument transformation must be a symbol")

     ;; For finding the definition of this rule
     (defalias ',name #'ignore
       "Open rule for phony.")

     (phony--add-rule
      (make-phony--open-rule
       :name ',name
       :external-name ,(or external-name (phony--to-python-identifier name))
       :transformation ,transformation
       :alternatives ,alternatives))

     (seq-doseq (to (ensure-list ',contributes-to))
       (phony--add-alternative ',name to))

     ',name))

(cl-defstruct phony--element-literal
  "Element matching a literal utterance."
  (string nil
          :type string
          :documentation "Utterance that matches this element."))

(cl-defstruct phony--element-compound
  "Element matching a sequence of sub-elements."
  forms)

(cl-defstruct (phony--element-optional
               (:include phony--element-compound))
  "Element matching sub-elements zero or one times.")

(cl-defstruct (phony--element-repeat
               (:include phony--element-compound))
  "Element matching sub-elements potentially multiple times.")

(cl-defstruct (phony--element-one-or-more
               (:include phony--element-repeat))
  "Element matching sub-elements one or more times.")

(cl-defstruct (phony--element-zero-or-more
               (:include phony--element-repeat))
  "Element matching sub-elements zero or more times.")

(cl-defstruct phony--element-argument
  "Element matching FORM and binding it to function argument NAME."
  (name nil
        :type symbol
        :documentation "Symbol naming the argument that captures the value of the match.")
  (form nil
        :type sexp
        :documentation "Form whose match will bind to the argument."))

(cl-defstruct phony--element-external-rule
  "Element matching some external rule.
Currently only relevant for the talon exporter."
  name namespace)

(cl-defstruct phony--element-rule
  "Element matching another rule."
  (name nil
        :type symbol
        :documentation "Symbol naming the phony rule that this element matches."))

(defun phony--element-children (element)
  "Return all direct sub-elements of ELEMENT."
  (cond
   ((phony--element-compound-p element)
    (phony--element-compound-forms element))
   ((phony--element-argument-p element)
    (list (phony--element-argument-form element)))
   (t nil)))

(defun phony--parse-speech-value-element (element)
  (cond
   ((symbolp element) (make-phony--element-rule
                       :name element))
   ((eq (car element) 'external-rule) (make-phony--element-external-rule
                                       :name (car (last (cdr element)))
                                       :namespace (butlast (cdr element))))
   ((symbolp (car element))
    (error "Not an argument nor element type: `%S' in form `%S'"
           (car element)
           element))
   (t (error "No parse for %S" element))))

(defun phony--parse-speech-element (element arglist)
  (cond
   ((stringp element) (make-phony--element-literal
                       :string element))
   ((member (car-safe element) arglist) (make-phony--element-argument
                                    :name (car element)
                                    :form (phony--parse-speech-value-element
                                           (cadr element))))
   ;; NOTE: The reader interprets ? as a character escape, so to use
   ;; it in the specification of the pattern we actually need to
   ;; match on the character after that, which we require to be
   ;; space.
   ((eq (car-safe element) ?\s) (make-phony--element-optional
                                 :forms (mapcar
                                         (lambda (subelement)
                                           (phony--parse-speech-element
                                            subelement arglist))
                                         (cdr element))))
   ((eq (car-safe element) '+) (make-phony--element-one-or-more
                                :forms (mapcar
                                        (lambda (subelement)
                                          (phony--parse-speech-element
                                           subelement arglist))
                                        (cdr element))))
   ((eq (car-safe element) '*) (make-phony--element-zero-or-more
                                :forms (mapcar
                                        (lambda (subelement)
                                          (phony--parse-speech-element
                                           subelement arglist))
                                        (cdr element))))
   (t (phony--parse-speech-value-element element))))

(cl-defgeneric phony--collect (predicate element)
  (let ((collected-children
         (phony--collect
          predicate
          (phony--element-children element))))
    (if (funcall predicate element)
        (cons element collected-children)
      collected-children)))

(cl-defmethod phony--collect (predicate (element-list list))
  (apply #'append
         (seq-map
          (lambda (element) (phony--collect predicate element))
          element-list)))

(defun phony--find-variable-element (argument elements)
  (seq-find (lambda (variable)
              (eq
               (phony--element-argument-name variable)
               argument))
            (phony--collect
             #'phony--element-argument-p
             elements)))

(defun phony--dependencies (rule-or-name)
  (phony--dependencies-implementation
   (phony--normalize-rule rule-or-name)))

(cl-defgeneric phony--dependencies-implementation (rule)
  "Return name of rules that RULE depends on.")

(cl-defmethod phony--dependencies-implementation ((rule phony--procedure-rule))
  "Return list of rule RULE depends on.

Rules in the list occur the same amount of times they are referenced in
RULE."
  (seq-map (lambda (element)
             (phony--element-rule-name element))
           (phony--collect
            #'phony--element-rule-p
            (phony--procedure-rule-elements rule))))

(cl-defmethod phony--dependencies-implementation ((rule phony--open-rule))
  (phony--open-rule-alternatives rule))

(cl-defmethod phony--dependencies-implementation ((_rule phony--dictionary))
  '())

(cl-defstruct (phony--dependency-data
               (:constructor nil)
               (:constructor phony--make-dependency-data))
  ;; DEPENDENCIES and DEPENDENTS should contain multiple occurrences
  ;; of a dependency if the rule refers to the dependancy multiple
  ;; times.
  (forward (make-hash-table))
  (backward (make-hash-table))
  (cycle nil)
  (contains-errors nil)
  (linear-extension nil))

(defun phony--populate-dependency-graph (dependency-data)
  (let ((dependencies (make-hash-table))
        (dependents (make-hash-table)))
    (seq-doseq (rule (phony--get-rules))
      (puthash rule '() dependencies)
      (puthash rule '() dependents))
    (seq-doseq (rule (phony--get-rules))
      (seq-doseq (dependency (phony--dependencies rule))
        (if (not (phony--get-rule dependency))
            (progn
              (display-warning 'phony
                               (format "Rule %S (referenced in %S) is not defined"
                                       dependency (phony--rule-name rule)))
              (setf (phony--dependency-data-contains-errors dependency-data) t))
          (push (phony--get-rule dependency) (gethash rule dependencies))
          (push rule (gethash dependency dependents)))))
    (setf (phony--dependency-data-forward dependency-data)
          dependencies)
    (setf (phony--dependency-data-backward dependency-data)
          dependents)))

(defun phony--dfs-analysis (rule visited finished dependency-data)
  "Perform depth-first-search to collect data into dependency-data."
  (let ((successors (phony--dependency-data-forward dependency-data)))
    (cl-block nil
      (when (gethash rule finished)
        (cl-return nil))
      (when (gethash rule visited)
        (cl-return (list rule)))

      (puthash rule t visited)
      (seq-doseq (successor (gethash rule successors))
        (when-let ((path (phony--dfs-analysis successor visited finished dependency-data)))
          (if (and (not (length= path 1))
                   (eq (seq-first path) (car (last path))))
              (cl-return path)
            (cl-return (cons rule path)))))

      (puthash rule t finished)
      (push rule (phony--dependency-data-linear-extension dependency-data))
      (cl-return nil))))

(cl-defun phony--try-finding-dependency-cycle (dependency-data)
  (let ((visited (make-hash-table))
        (finished (make-hash-table)))
    (seq-doseq (rule (phony--get-rules))
      (when-let ((cycle (phony--dfs-analysis
                         rule visited finished
                         dependency-data)))
        (setf (phony--dependency-data-cycle dependency-data)
              (seq-map #'phony--rule-name cycle))
        (cl-return-from phony--try-finding-dependency-cycle)))))

(defvar phony--last-analysis nil)

(defun phony--analyze-grammar ()
  (let ((dependency-data (phony--make-dependency-data)))
    (phony--populate-dependency-graph dependency-data)
    (phony--try-finding-dependency-cycle dependency-data)
    (setf (phony--dependency-data-linear-extension dependency-data)
          (reverse
           (phony--dependency-data-linear-extension dependency-data)))
    (when-let (cycle (phony--dependency-data-cycle dependency-data))
      (display-warning 'phony (concat "Cycle found: "
                                      (string-join
                                       (seq-map #'symbol-name cycle)
                                       " -> ")))
      (setf (phony--dependency-data-contains-errors dependency-data) t))
    (setq phony--last-analysis dependency-data)
    dependency-data))

(defun phony--dependents (rule-or-name)
  (gethash
   (phony--normalize-rule rule-or-name)
   (phony--dependency-data-backward phony--last-analysis)))

(defcustom phony-export-function #'phony-dragonfly-export
  "Function to be used for exporting spoken rules."
  ;; Using radio instead of choice did not display :tag
  :type '(choice (function-item :tag "Dragonfly exporter"
                                phony-dragonfly-export)
                 (function-item :tag "Talon exporter"
                                phony-talon-export))
  :tag "Phony Rule Exporter"
  :risky t
  :group 'phony)

(defun phony--export-all ()
  "Export all rules to the speech recognition engine."
  (let ((dependency-data (phony--analyze-grammar)))
    (if (phony--dependency-data-contains-errors dependency-data)
        (display-warning 'phony "Grammar contains errors, not exporting")
      (funcall phony-export-function dependency-data))))

(defun phony-request-export ()
  "Export all rules when next idle."
  (interactive)
  (cancel-function-timers #'phony--export-all)
  (run-with-idle-timer 0 nil #'phony--export-all))

(cl-defun phony--export-rule (function
                              arglist
                              speech-pattern
                              &key
                              (mode 'global)
                              (contributes-to nil)
                              (external-name nil)
                              (export t)
                              (anchor-beginning nil)
                              (anchor-end nil))
  (setq arglist (byte-compile-arglist-vars arglist))
  (setq mode (ensure-list mode))
  (setq external-name (or external-name (phony--to-python-identifier function)))
  (phony-remove-rule function)
  (let* ((elements
          (seq-map (lambda (element)
                     (phony--parse-speech-element element (byte-compile-arglist-vars arglist)))
                   speech-pattern)))
    (phony--add-rule
     (make-phony--procedure-rule
      :name function
      :external-name external-name
      :function function
      :elements elements
      :arglist arglist
      :modes mode
      :export export
      :anchor-beginning-p anchor-beginning
      :anchor-end-p anchor-end))

    (seq-doseq (to (ensure-list contributes-to))
      (phony--add-alternative function to))

    (phony-request-export)))

(defun phony--export-rule-declare (function arglist &rest speech-pattern)
  `(phony--export-rule #',function
                       ',arglist
                       ',speech-pattern))

(cl-defun phony--speech-declaration (function arglist &rest declaration-args)
  (let* ((split-args (phony--split-keywords-rest declaration-args))
         (keyword-arguments (car split-args))
         (pattern (cdr split-args)))
    `(phony--export-rule
      #',function
      ',arglist
      ',pattern
      ,@keyword-arguments)))

(setf (alist-get 'phony-rule defun-declarations-alist)
      (list #'phony--speech-declaration))

(defmacro phony-rule (args)
  "Declare function to be a rule invokeable by voice.

This form must occur inside a `declare' form to take effect.

ELEMENTS is a sequence of elements declaring when this rule gets
matched.  An element may have one of the following forms:

  STRING           Match a literal STRING.
  SYMBOL           Match a rule named SYMBOL.
  (ARG SYMBOL)     Match a rule named SYMBOL, bind its value to argument
                   ARG.  ARG must be part of the function's arglist.
  (? ELEMS...)     Optionally match elements ELEMS.
  (* ELEMS...)     Match elements ELEMS zero or more times.
  (+ ELEMS...)     Match elements ELEMS one or more times.

Optional arguments for the rule are given before ELEMENTS as a sequence
of alternating KEY and VALUE.  Optional arguments are:

  :export            If nil, this rule cannot be spoken directly but may
                     occur as part of other rules.  Default is t.
  :mode              A mode or list of modes for this rule should be
                     active.  Only relevant for exported rules.  Default
                     is 'global.
  :contributes-to    A symbol or list of symbols of open rules that this
                     procedure should contribute to.  See
                     `phony-define-open-rule' for open rules.  Default
                     is nil.
  :anchor-beginning  If t, this rule must occur first in an utterance.
                     Default is nil.
  :anchor-end        If t, this rule must occur last in an utterance.
                     Default is nil.

\(fn [KEY VALUE]... ELEMENTS...)"
  `(message "Stray `phony-rule' form: %S" '(phony-rule . ,args)))

(require 'phony-talon)
(require 'phony-dragonfly)

(provide 'phony)
;;; phony.el ends here
