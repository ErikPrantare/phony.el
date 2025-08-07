;;; phony.el --- Speech bindings for Elisp           -*- lexical-binding: t; -*-

;; Copyright (C) 2024, 2025 Erik Präntare

;; Author: Erik Präntare
;; Keywords: files
;; Version: 0.1.0
;; Homepage: https://github.com/ErikPrantare/phony.el
;; Package-Requires: ((emacs "27.1"))
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

(defgroup phony nil
  "Functionality for defining speech bindings."
  :group 'files)

(defcustom phony-dictionaries-output-file nil
  "Output file for defined lists."
  :type 'file
  :group 'phony)

(defun phony--to-python-identifier (symbol)
  (concat "phony_" (replace-regexp-in-string (rx (not alnum)) "_" (symbol-name symbol))))

(cl-defstruct phony--rule-provisional-super
  "A rule for matching an utterance.

NAME is the name of the rule.

EXTERNAL-NAME is the name this rule will have for the speech recognition
engine, and should be a string."
  (name nil :type symbol)
  (external-name nil :type string))

(cl-defstruct (phony--rule
               (:include phony--rule-provisional-super))
  (modes '(global)) (export nil))

(cl-defstruct (phony--procedure-rule
               (:include phony--rule))
  function elements arglist)

(cl-defstruct (phony--open-rule
               (:include phony--rule))
  (alternatives nil) (transformation nil))

(cl-defstruct (phony--dictionary
               (:include phony--rule-provisional-super)
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
  (symbol-value (phony--dictionary-name dictionary)))

(defun phony--normalize-rule (rule-or-name)
  (if (symbolp rule-or-name)
      (phony--get-rule rule-or-name)
    rule-or-name))

(defun phony--external-name (rule-or-name)
  (phony--rule-provisional-super-external-name
   (phony--normalize-rule rule-or-name)))

(defvar phony--rules (make-hash-table))

(defun phony--get-rules ()
  (hash-table-values phony--rules))

(defun phony--get-rule (name)
  (gethash name phony--rules))

(defun phony--add-rule (rule)
  (puthash (phony--rule-provisional-super-name rule)
           rule
           phony--rules))

(defun phony-remove-rule (rule-name)
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
  (alist-get utterance dictionary nil nil #'equal))

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
  "Create lookup string for UTTERANCE in DICTIONARY.

When evaluating the returned value from emacsclient, this
performs the lookup."
  (if (phony--dictionary-format-raw-p dictionary)
      (cdr entry)
    (format "(phony-dictionary-get \"%s\" %s)"
            (car entry)
            (phony--dictionary-name dictionary))))

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
  "Send dictionaries to `phony-dictionaries-output-file'.

Talon can read this file to register the dictionaries."
  (with-temp-file phony-dictionaries-output-file
    (json-insert
     (mapcar #'phony--prepare-dictionary-for-serialization
             (seq-filter #'phony--dictionary-p (phony--get-rules))))
    (json-pretty-print-buffer)))

(defun phony--request-sync-dictionaries ()
  "Sync DICTIONARY-NAMES when next idle."
  (interactive)
  (cancel-function-timers #'phony--send-dictionaries)
  (run-with-idle-timer 0.0 nil #'phony--send-dictionaries))

(defun phony--dictionary-variable-watcher (symbol new-value operation buffer)
  (phony--request-sync-dictionaries))

(cl-defun phony--define-dictionary (name mapping &key (external-name nil) (format-raw nil))
  (setq external-name (or external-name
                          (phony--to-python-identifier name)))

  (unless (proper-list-p mapping)
    (error "The mapping of %s must be a list" name))

  (when-let ((non-cons (seq-find (lambda (x) (not (consp x))) mapping)))
    (error "The mapping of %s must be a alist, but %s is not a cons cell"
           name non-cons))

  (when-let ((non-string-key (seq-find
                              (lambda (entry) (not (stringp (car-safe entry))))
                              mapping)))
    (error "The keys of %s must be strings, but %s is not a string"
           name (car non-string-key)))

  (phony--add-rule
   (phony--make-dictionary
    name
    :external-name external-name
    :format-raw-p format-raw))
  (phony--request-sync-dictionaries)

  ;; TODO: Do the above sanity check on updates as well
  (unless (memq #'phony--dictionary-variable-watcher (get-variable-watchers name))
    (add-variable-watcher name #'phony--dictionary-variable-watcher))

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
Update `phony-dictionaries-output-file' to contain the definition.

ALIST is an alist mapping utterances to values.  An utterance
is a string containing the spoken form for referencing the value.

ALIST will be stored in a variable named NAME."
  (declare (indent defun))
  (let ((split-arguments (phony--split-keywords-rest arguments)))
    ;; We need to expand to defvar, or else xref will not find the
    ;; definition.  defvar only modifies the variable when it is void,
    ;; so if it is not we revert to setq.
    `(,(if (boundp name) 'setq 'defvar)
      ,name
      (phony--define-dictionary ',name ,@(cdr split-arguments) ,@(car split-arguments)))))

(defun phony--add-alternative (alternative open-rule-name)
  (let ((rule (phony--get-rule open-rule-name)))
    (unless rule
      (error "No rule %s defined" open-rule-name))
    (unless (phony--open-rule-p rule)
      (error "No open rule %s defined" open-rule-name))
    (cl-pushnew alternative (phony--open-rule-alternatives rule))))

(cl-defmacro phony-define-open-rule (name &key
                                          (talon-name nil)
                                          (contributes-to nil)
                                          (transformation nil)
                                          (export nil))
  (declare (indent defun))
  `(progn
     ,@(seq-filter
        #'identity
        (list
         ;; Code generation currently assumes that the transformation
         ;; is given as a function symbol
         `(cl-assert (symbolp ,transformation) nil
                     "Argument transformation must be a symbol")
         `(phony--add-rule
           (make-phony--open-rule
            :name ',name
            :external-name ,(or talon-name
                                (phony--to-python-identifier name))
            :transformation ,transformation
            :export ,export))
         `(seq-doseq (to (ensure-list ',contributes-to))
            (phony--add-alternative
             ',name
             to))
         `',name))))

(cl-defstruct phony--element-literal
  string)

(cl-defstruct phony--element-compound
  forms)

(cl-defstruct (phony--element-optional
               (:include phony--element-compound)))

(cl-defstruct (phony--element-repeat
               (:include phony--element-compound)))

(cl-defstruct (phony--element-one-or-more
               (:include phony--element-repeat)))

(cl-defstruct (phony--element-zero-or-more
               (:include phony--element-repeat)))

(cl-defstruct phony--element-dictionary
  name)

(cl-defstruct phony--element-argument
  name form)

(cl-defstruct phony--element-external-rule
  name namespace)

(cl-defstruct phony--element-rule
  name)

(defun phony--ast-children (element)
  (cond
   ((phony--element-compound-p element)
    (phony--element-compound-forms element))
   ((phony--element-argument-p element)
    (list (phony--element-argument-form element)))
   (t nil)))

(defun phony--parse-speech-element (element arglist)
  (cond
   ((stringp element) (make-phony--element-literal
                       :string element))
   ((member (car element) arglist) (make-phony--element-argument
                                    :name (car element)
                                    :form (phony--parse-speech-element (cadr element) arglist)))
   ;; NOTE: The reader interprets ? as a character escape, so to use
   ;; it in the specification of the pattern we actually need to
   ;; match on the character after that, which we require to be
   ;; space.
   ((eq (car element) ?\s) (make-phony--element-optional
                            :forms (mapcar
                                    (lambda (subelement)
                                      (phony--parse-speech-element
                                       subelement arglist))
                                    (cdr element))))
   ((eq (car element) '+) (make-phony--element-one-or-more
                           :forms (mapcar
                                   (lambda (subelement)
                                     (phony--parse-speech-element
                                      subelement arglist))
                                   (cdr element))))
   ((eq (car element) '*) (make-phony--element-zero-or-more
                           :forms (mapcar
                                   (lambda (subelement)
                                     (phony--parse-speech-element
                                      subelement arglist))
                                   (cdr element))))
   ((eq (car element) 'rule) (make-phony--element-rule
                              :name (cadr element)))
   ((eq (car element) 'talon-capture) (make-phony--element-external-rule
                                       :name (car (last (cdr element)))
                                       :namespace (butlast (cdr element))))
   ((eq (car element) 'list) (make-phony--element-dictionary
                              :name (cadr element)))
   (t (error (format "No parse for %S" element)))))

(cl-defgeneric phony--collect (predicate element)
  (let ((collected-children
         (phony--collect
          predicate
          (phony--ast-children element))))
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

(cl-defgeneric phony--dependencies (rule))

(cl-defmethod phony--dependencies ((rule phony--procedure-rule))
  "Return list of RULES rule depends on.

Rules in the list occur the same amount of times they are referenced in
RULE."
  (seq-map (lambda (element)
             (phony--get-rule
              (cond
               ((phony--element-rule-p element)
                (phony--element-rule-name element))
               ((phony--element-dictionary-p element)
                (phony--element-dictionary-name element)))))
           (phony--collect
            (lambda (element)
              (or (phony--element-rule-p element)
                  (phony--element-dictionary-p element)))
            (phony--procedure-rule-elements rule))))

(cl-defmethod phony--dependencies ((rule phony--open-rule))
  (seq-map #'phony--get-rule
           (phony--open-rule-alternatives rule)))

(cl-defmethod phony--dependencies ((rule phony--dictionary))
  '())

(cl-defstruct (phony--analysis-data
               (:constructor nil)
               (:constructor phony--make-analysis-data))
  ;; DEPENDENCIES and DEPENDENTS should contain multiple occurrences
  ;; of a dependency if the rule refers to the dependancy multiple
  ;; times.
  (dependencies (make-hash-table))
  (dependents (make-hash-table))
  (dependency-cycle nil)
  (contains-errors nil))

(defun phony--populate-dependency-graph (analysis-data)
  (let ((dependencies (make-hash-table))
        (dependents (make-hash-table)))
    (seq-doseq (rule (phony--get-rules))
      (puthash rule '() dependencies)
      (puthash rule '() dependents))
    (seq-doseq (rule (phony--get-rules))
      (seq-doseq (dependency (phony--dependencies rule))
        (push dependency (gethash rule dependencies))
        (push rule (gethash dependency dependents))))
    (setf (phony--analysis-data-dependencies analysis-data)
          dependencies)
    (setf (phony--analysis-data-dependents analysis-data)
          dependents)))

(defun phony--find-cycle (rule visited finished successors)
  "Return path of dependency cycle if detected, or nil otherwise."
  (cl-block nil
    (when (gethash rule finished)
      (cl-return nil))
    (when (gethash rule visited)
      (cl-return (list rule)))

    (puthash rule t visited)
    (seq-doseq (successor (gethash rule successors))
      (when-let ((path (phony--find-cycle successor visited finished successors)))
        (if (and (not (length= path 1))
                 (eq (seq-first path) (car (last path))))
            (cl-return path)
          (cl-return (cons rule path)))))
    (puthash rule t finished)
    (cl-return nil)))

(cl-defun phony--try-finding-dependency-cycle (analysis-data)
  (let ((visited (make-hash-table))
        (finished (make-hash-table)))
    (seq-doseq (rule (phony--get-rules))
      (when-let ((cycle (phony--find-cycle
                         rule visited finished
                         (phony--analysis-data-dependencies analysis-data))))
        (setf (phony--analysis-data-dependency-cycle analysis-data)
              (seq-map #'phony--rule-name cycle))
        (cl-return-from phony--try-finding-dependency-cycle)))))

(defun phony--analyze-grammar ()
  (let ((analysis-data (phony--make-analysis-data)))
    (phony--populate-dependency-graph analysis-data)
    (phony--try-finding-dependency-cycle analysis-data)
    (when-let (cycle (phony--analysis-data-dependency-cycle analysis-data))
      (warn "Cycle found: %s" cycle)
      (setf (phony--analysis-data-contains-errors analysis-data) t))
    analysis-data))

(defvar phony-export-function nil)

(defun phony--export-all ()
  (let ((analysis-data (phony--analyze-grammar)))
    (if (phony--analysis-data-contains-errors analysis-data)
        (warn "Grammar contains errors, not exporting")
      (funcall phony-export-function))))

(defun phony-request-export ()
  (interactive)
  (cancel-function-timers #'phony--export-all)
  (run-with-idle-timer 0 nil #'phony--export-all))

(cl-defun phony--export-rule (function
                              arglist
                              speech-pattern
                              &key
                              (mode 'global)
                              (talon-name nil)
                              (contributes-to nil)
                              (export t))
  (setq arglist (byte-compile-arglist-vars arglist))
  (setq mode (ensure-list mode))
  (phony-remove-rule function)
  (let* ((elements
          (seq-map (lambda (element)
                     (phony--parse-speech-element element (byte-compile-arglist-vars arglist)))
                   speech-pattern)))
    (phony--add-rule
     (make-phony--procedure-rule
      :name function
      :function function
      :elements elements
      :arglist arglist
      :modes mode
      :external-name (or talon-name
                         (phony--to-python-identifier function))
      :export export))

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

(provide 'phony)
;;; phony.el ends here
