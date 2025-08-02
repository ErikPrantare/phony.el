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

(defun phony-dictionary-get (utterance dictionary)
  "Return the value corresponding to UTTERANCE in LIST.
If no such value exists, return nil."
  (alist-get utterance dictionary nil nil #'equal))

(defmacro phony-dictionary-put (utterance dictionary value)
  "Set the value of UTTERANCE in LIST to VALUE.
If value is nil, remove the utterance from the list instead.

Invoking this function will sync the list with talon."
  (declare (indent defun))
  `(prog1
       (setf (alist-get ,utterance ,dictionary nil t #'equal) ,value)
     (phony--request-sync (list ',dictionary))))

(gv-define-expander phony-dictionary-get
  ;; We need to use `gv-define-expander', because the simpler versions
  ;; expand to a let-expression binding the dictionary to a local
  ;; variable.  That meant removing elements became impossible.
  (lambda (do utterance dictionary)
    (funcall do `(phony-dictionary-get ,utterance ,dictionary)
             (lambda (value)
               `(phony-dictionary-put ,utterance ,dictionary ,value)))))

(defun phony--create-lookup-representation (entry dictionary-name)
  "Create lookup string for UTTERANCE in DICTIONARY-NAME.

When evaluating the returned value from emacsclient, this
performs the lookup."
  (if (get dictionary-name 'phony--format-raw)
      (cdr entry)
    (format "(phony-dictionary-get \"%s\" %s)"
            (car entry)
            dictionary-name)))

(defun phony--prepare-dictionary-for-serialization (dictionary-name)
  "Return dictionary in DICTIONARY-NAME as an entry for `json-serialize'.

This represents one key-value pair, mapping the talon dictionary name to
its dictionary.  `json-serialize' will create a JSON object passed a
dictionary of such key-value pairs."
  (cons (phony--dictionary-external-name dictionary-name)
        (let ((mapping (symbol-value dictionary-name)))
          (mapcar (lambda (entry)
                    (cons
                     ;; json-serialize expects symbols for keys.
                     (make-symbol (car entry))
                     ;; Each value is a string, encoding a form that
                     ;; will evaluate to the actual value.
                     (phony--create-lookup-representation
                      entry dictionary-name)))
                  mapping))))

;; TODO: Handle IO errors
(defun phony--send-dictionaries (dictionary-names)
  "Send dictionaries DICTIONARY-NAMES to `phony-dictionaries-output-file'.

Talon can read this file to register the dictionaries."
  (with-temp-file phony-dictionaries-output-file
    (json-insert
     (mapcar #'phony--prepare-dictionary-for-serialization dictionary-names))))

(defvar phony--dictionary-names '()
  "All defined talon dictionaries.")

(defun phony--request-sync (&optional dictionary-names)
  "Sync DICTIONARY-NAMES when next idle."
  (interactive)
  ;; For now, we are always resync everything.
  (cancel-function-timers #'phony--send-dictionaries)
  (run-with-idle-timer 0.0 nil #'phony--send-dictionaries phony--dictionary-names))

(defun phony--dictionary-external-name (dictionary-name)
  (get dictionary-name 'phony--talon-name))

(defun phony--to-python-identifier (symbol)
  (concat "phony_" (replace-regexp-in-string (rx (not alnum)) "_" (symbol-name symbol))))

(cl-defun phony--define-dictionary (name mapping &key (external-name nil) (format-raw nil))
  (setq external-name (intern
                       (or external-name
                           (phony--to-python-identifier name))))
  ;; TODO: Put all properties in a hash
  (put name 'phony--talon-name external-name)
  (put name 'phony--format-raw format-raw)

  (add-to-list 'phony--dictionary-names name)
  (phony--request-sync phony--dictionary-names)

  ;; Needs to return the actual mapping, see `phony-define-list'
  mapping)

(defun phony--define-list (dictionary-name talon-name mapping options)
  "Define list with LIST-NAME and TALON-NAME containing MAPPING.
Update `phony-dictionaries-output-file' to contain the definition."
  (phony--define-dictionary
   dictionary-name
   mapping
   :external-name (symbol-name talon-name)
   :format-raw (plist-get options :format-raw)))

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
  (declare (indent defun))
  (let ((split-arguments (phony--split-keywords-rest arguments)))
    ;; We need to expand to defvar, or else xref will not find the
    ;; definition.  defvar only modifies the variable when it is void,
    ;; so if it is not we revert to setq.
  `(,(if (boundp name) 'setq 'defvar)
    ,name
    (phony--define-dictionary ',name ,@(cdr split-arguments) ,@(car split-arguments)))))

(defmacro phony-define-list (list-name talon-name mapping &rest options)
  "Define a LIST-NAME with TALON-NAME containing MAPPING.
Update `phony-dictionaries-output-file' to contain the definition.

MAPPING is an alist mapping utterances to values.  An utterance
is a string containing the spoken form for referencing the value.

MAPPING will be stored in the variable LIST."
  (declare (indent defun)
           (obsolete phony-define-dictionary "0.1.0"))
  ;; We need to expand to defvar, or else xref will not find the
  ;; definition.  defvar only modifies the variable when it is void,
  ;; so if it is not we revert to setq.
  `(,(if (boundp list-name) 'setq 'defvar)
    ,list-name
    (phony--define-list ',list-name ',talon-name ,mapping ',options)))

(cl-defstruct phony--rule
  name external-name (modes '(global)) (export nil))

(cl-defstruct (phony--procedure-rule
               (:include phony--rule))
  function elements arglist)

(cl-defstruct (phony--open-rule
               (:include phony--rule))
  (alternatives nil) (transformation nil))

(defun phony--add-alternative (alternative open-rule-name)
  (unless (gethash open-rule-name phony--rules)
    (error "No rule %s defined" open-rule-name))
  (unless (phony--open-rule-p
           (gethash open-rule-name phony--rules))
    (error "No open rule %s defined" open-rule-name))
  (cl-pushnew alternative
              (phony--open-rule-alternatives
               (gethash open-rule-name phony--rules))))

(defvar phony--rules (make-hash-table))

(defun phony--rules ()
  (hash-table-values phony--rules))

(defun phony--get-rule (name)
  (cond
   ((gethash name phony--rules)
    (gethash name phony--rules))
   ((get name 'phony--talon-name)
    (symbol-value name))
   (t (error "No rule with name %S" name))))

(defun phony-remove-rule (rule-name)
  (interactive (list (intern (completing-read "Remove rule: "
                                              (hash-table-keys
                                               phony--rules)))))
  (remhash rule-name phony--rules)
  (seq-doseq (open-rule (seq-filter
                         #'phony--open-rule-p
                         (hash-table-values phony--rules)))
    (setf (phony--open-rule-alternatives open-rule)
          (remove rule-name (phony--open-rule-alternatives open-rule))))
  (phony-request-export))

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
         `(puthash ',name
                   (make-phony--open-rule
                    :name ',name
                    :external-name ,(or talon-name
                                        (phony--to-python-identifier name))
                    :transformation ,transformation
                    :export ,export)
                   phony--rules)
         (when contributes-to
           (if (listp contributes-to)
               `(seq-doseq (to ',contributes-to)
                  (phony--add-alternative
                 ',name
                 to))
             `(phony--add-alternative
               ',name
               ',contributes-to)))
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

(cl-defmethod phony--dependencies ((rule list))
  ;; In this case the rule is a dictionary
  nil)

(defun phony--contains-cycle-search (rule visited finished)
  "Return list of dependency cycle if detected, or nil otherwise."
  (cl-block nil
    (when (gethash rule finished)
      (cl-return nil))
    (when (gethash rule visited)
      (cl-return (list rule)))

    (puthash rule t visited)
    (seq-doseq (successor (phony--dependencies rule))
      (when-let ((path (phony--contains-cycle-search successor visited finished)))
        (if (and (not (length= path 1))
                 (eq (seq-first path) (car (last path))))
            (cl-return path)
          (cl-return (cons rule path)))))
    (puthash rule t finished)
    (cl-return nil)))

(cl-defun phony--contains-cycle ()
  (let ((stack '())
        (visited (make-hash-table))
        (finished (make-hash-table)))
    (seq-doseq (rule (phony--rules))
      (when-let ((cycle (phony--contains-cycle-search
                         rule visited finished)))
        (warn "Cycle found: %s" (seq-map #'phony--rule-name cycle))
        (cl-return-from phony--contains-cycle t))))
  nil)

(defun phony--check-grammar-consistency ()
  (let ((contains-cycles (phony--contains-cycle)))
    (not contains-cycles)))

(defvar phony-export-function nil)

(defun phony--export-all ()
  (when (phony--check-grammar-consistency)
    (funcall phony-export-function)))

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
    (puthash function
             (make-phony--procedure-rule
              :name function
              :function function
              :elements elements
              :arglist arglist
              :modes mode
              :external-name (or talon-name
                                 (phony--to-python-identifier function))
              :export export)
             phony--rules)

    (when contributes-to
      (phony--add-alternative function contributes-to)
      (unless (gethash contributes-to phony--rules)
        (error "No rule %s defined" contributes-to))
      (unless (phony--open-rule-p
               (gethash contributes-to phony--rules))
        (error "No open rule %s defined" contributes-to))
      (cl-pushnew function
                  (phony--open-rule-alternatives
                   (gethash contributes-to phony--rules))))

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
