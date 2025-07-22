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
  "Functionality for defining talon lists in Emacs Lisp."
  :group 'files)

(defcustom phony-output-file "~/.talon/emacs-lists.json"
  "Output file for defined lists."
  :type 'file
  :group 'phony)

(defun phony-list-get (utterance list)
  "Return the value corresponding to UTTERANCE in LIST.
If no such value exists, return nil."
  (alist-get utterance list nil nil #'equal))

(defmacro phony-list-put (utterance list value)
  "Set the value of UTTERANCE in LIST to VALUE.
If value is nil, remove the utterance from the list instead.

Invoking this function will sync the list with talon."
  (declare (indent defun))
  `(prog1
       (setf (alist-get ,utterance ,list nil t #'equal) ,value)
     (phony--request-sync (list ',list))))

(gv-define-expander phony-list-get
  ;; We need to use `gv-define-expander', because the simpler versions
  ;; expand to a let-expression binding the list to a local variable.
  ;; That meant removing elements became impossible.
  (lambda (do utterance list)
    (funcall do `(phony-list-get ,utterance ,list)
             (lambda (value)
               `(phony-list-put ,utterance ,list ,value)))))

(defun phony--create-lookup-representation (entry list-name)
  "Create lookup string for UTTERANCE in LIST-NAME.

When evaluating the returned value from emacsclient, this
performs the lookup."
  (if (get list-name 'phony--format-raw)
      (cdr entry)
    (format "(phony-list-get \"%s\" %s)"
            (car entry)
            list-name)))

(defun phony--prepare-list-for-serialization (list-name)
  "Return list in LIST-NAME as an entry for `json-serialize'.

This represents one key-value pair, mapping the talon list name
to its list.  `json-serialize' will create a JSON object
passed a list of such key-value pairs."
  (cons (get list-name 'phony--talon-name)
        (let ((mapping (symbol-value list-name)))
          (mapcar (lambda (entry)
                    (cons
                     ;; json-serialize expects symbols for keys.
                     (make-symbol (car entry))
                     ;; Each value is a string, encoding a form that
                     ;; will evaluate to the actual value.
                     (phony--create-lookup-representation
                      entry list-name)))
                  mapping))))

;; TODO: Handle IO errors
(defun phony--send-lists (list-names)
  "Send lists coded in LIST-NAMES to `phony-output-file'.

Talon can read this file to register the lists."
  (with-temp-file phony-output-file
    (json-insert
     (mapcar #'phony--prepare-list-for-serialization list-names))))

(defvar phony--list-names '()
  "All defined talon lists.")

(defun phony--request-sync (&optional list-names)
  "Sync LIST-NAMES when next idle."
  (interactive)
  ;; For now, we are always resync everything.
  (cancel-function-timers #'phony--send-lists)
  (run-with-idle-timer 0.0 nil #'phony--send-lists phony--list-names))

(defun phony--define-list (list-name talon-name mapping options)
  "Define list with LIST-NAME and TALON-NAME containing MAPPING.
Update `phony-output-file' to contain the definition."

  ;; TODO: Put all properties in a hash
  (put list-name 'phony--talon-name talon-name)
  (put list-name 'phony--format-raw (plist-get options :format-raw))

  (add-to-list 'phony--list-names list-name)
  (phony--request-sync phony--list-names)

  ;; Needs to return the actual mapping, see `phony-define-list'
  mapping)

(defmacro phony-define-list (list-name talon-name mapping &rest options)
  "Define a LIST-NAME with TALON-NAME containing MAPPING.
Update `phony-output-file' to contain the definition.

MAPPING is an alist mapping utterances to values.  An utterance
is a string containing the spoken form for referencing the value.

MAPPING will be stored in the variable LIST."
  (declare (indent defun))
  ;; We need to expand to defvar, or else xref will not find the
  ;; definition.  defvar only modifies the variable when it is void,
  ;; so if it is not we revert to setq.
  `(,(if (boundp list-name) 'setq 'defvar)
    ,list-name
    (phony--define-list ',list-name ',talon-name ,mapping ',options)))

(cl-defstruct phony--rule
  talon-name (modes '(global)) (export nil))

(cl-defstruct (phony--procedure-rule
               (:include phony--rule))
  function components arglist)

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

(defun phony--get-rule (name)
  (or (gethash name phony--rules)
      (error "No rule with name %S" name)))

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
                    :talon-name ,(or talon-name
                                     (phony--to-python-identifier name))
                    :transformation ,transformation
                    :export ,export)
                   phony--rules)
         (when contributes-to
           `(phony--add-alternative
             ',name
             ',contributes-to))
         `',name))))

(cl-defstruct phony--ast-literal
  string)

(cl-defstruct phony--compound-component
  forms)

(cl-defstruct (phony--ast-optional
               (:include phony--compound-component)))

(cl-defstruct (phony--repeat-component
               (:include phony--compound-component)))

(cl-defstruct (phony--ast-one-or-more
               (:include phony--repeat-component)))

(cl-defstruct (phony--ast-zero-or-more
               (:include phony--repeat-component)))

(cl-defstruct phony--ast-element
  list)

(cl-defstruct phony--ast-variable
  argument form)

(cl-defstruct phony--ast-talon-capture
  name namespace)

(cl-defstruct phony--ast-rule
  name)

(cl-defgeneric phony--ast-talon-name (component)
  (error "No talon name for component %S" component))

(cl-defmethod phony--ast-talon-name ((component phony--ast-rule))
  (phony--rule-talon-name
   (phony--get-rule
    (phony--ast-rule-name component))))

(cl-defmethod phony--ast-talon-name ((component phony--ast-talon-capture))
  (phony--ast-talon-capture-name component))

(cl-defmethod phony--ast-talon-name ((component phony--ast-element))
  (get
   (phony--ast-element-list component)
   'phony--talon-name))

(defun phony--ast-children (component)
  (cond
   ((phony--compound-component-p component)
    (phony--compound-component-forms component))
   ((phony--ast-variable-p component)
    (list (phony--ast-variable-form component)))
   (t nil)))

(defun phony--parse-speech-component (component arglist)
  (cond
   ((stringp component) (make-phony--ast-literal
                         :string component))
   ((symbolp component) (make-phony--ast-element
                         :list component))
   ((member (car component) arglist) (make-phony--ast-variable
                                      :argument (car component)
                                      :form (phony--parse-speech-component (cadr component) arglist)))
   ;; NOTE: The reader interprets ? as a character escape, so to use
   ;; it in the specification of the pattern we actually need to
   ;; match on the character after that, which we require to be
   ;; space.
   ((eq (car component) ?\s) (make-phony--ast-optional
                              :forms (mapcar
                                      (lambda (subcomponent)
                                        (phony--parse-speech-component
                                         subcomponent arglist))
                                      (cdr component))))
   ((eq (car component) '+) (make-phony--ast-one-or-more
                             :forms (mapcar
                                     (lambda (subcomponent)
                                       (phony--parse-speech-component
                                        subcomponent arglist))
                                     (cdr component))))
   ((eq (car component) '*) (make-phony--ast-zero-or-more
                             :forms (mapcar
                                     (lambda (subcomponent)
                                       (phony--parse-speech-component
                                        subcomponent arglist))
                                     (cdr component))))
   ((eq (car component) 'rule) (make-phony--ast-rule
                                :name (cadr component)))
   ((eq (car component) 'talon-capture) (make-phony--ast-talon-capture
                                         :name (car (last (cdr component)))
                                         :namespace (butlast (cdr component))))
   ((eq (car component) 'list) (make-phony--ast-element
                                :list (cadr component)))
   (t (error (format "No parse for %S" component)))))

(cl-defgeneric phony--ast-match-string (component))

(cl-defmethod phony--ast-match-string ((component phony--ast-literal))
  (phony--ast-literal-string component))

(cl-defmethod phony--ast-match-string ((component phony--ast-element))
  (format "{user.%s}" (get (phony--ast-element-list component)
                           'phony--talon-name)))

(cl-defmethod phony--ast-match-string ((component phony--ast-optional))
  (format "[%s]" (phony--ast-match-string
                  (phony--ast-children component))))

(cl-defmethod phony--ast-match-string ((component phony--ast-one-or-more))
  (format "(%s)+" (phony--ast-match-string
                   (phony--ast-children component))))

(cl-defmethod phony--ast-match-string ((component phony--ast-zero-or-more))
  (format "(%s)*" (phony--ast-match-string
                   (phony--ast-children component))))

(cl-defmethod phony--ast-match-string ((component phony--ast-talon-capture))
  (format "<%s>" (string-join
                  (seq-map #'symbol-name
                           (append
                            (phony--ast-talon-capture-namespace component)
                            (list (phony--ast-talon-capture-name component))))
                  ".")))

(cl-defmethod phony--ast-match-string ((component phony--ast-rule))
  (format "<user.%s>"
          (phony--rule-talon-name
           (phony--get-rule
            (phony--ast-rule-name component)))))

(cl-defmethod phony--ast-match-string ((component phony--ast-variable))
  (phony--ast-match-string
   (phony--ast-variable-form component)))

(cl-defmethod phony--ast-match-string ((component-list list))
  (string-join (seq-map #'phony--ast-match-string
                        component-list)
               " "))

(cl-defgeneric phony--collect (predicate component)
  (let ((collected-children
         (phony--collect
          predicate
          (phony--ast-children component))))
    (if (funcall predicate component)
        (cons component collected-children)
      collected-children)))

(cl-defmethod phony--collect (predicate (component-list list))
  (apply #'append
         (seq-map
          (lambda (component) (phony--collect predicate component))
          component-list)))

(defun phony--find-variable-component (argument components)
  (seq-find (lambda (variable)
              (eq
               (phony--ast-variable-argument variable)
               argument))
            (phony--collect
             #'phony--ast-variable-p
             components)))

(cl-defgeneric phony--rule-talon-pattern (rule))

(cl-defmethod phony--rule-talon-pattern ((rule phony--procedure-rule))
  (let* ((components
          (phony--procedure-rule-components rule))
         (spoken-components
          (seq-map #'phony--ast-match-string components)))
    (string-join spoken-components " ")))

(cl-defmethod phony--rule-talon-pattern ((rule phony--open-rule))
  (string-join (seq-map
                (lambda (alternative)
                  (format "<user.%s>"
                          (phony--rule-talon-name
                           (gethash alternative phony--rules))))
                (phony--open-rule-alternatives rule))
               " | "))


(cl-defgeneric phony--variable-context (variable components)
  nil)

(cl-defmethod phony--variable-context (variable (components list))
  (thread-last
    components
    (seq-map (lambda (component) (phony--variable-context variable component)))
    (seq-find #'identity)))

(cl-defmethod phony--variable-context (variable (component phony--ast-variable))
  (when (eq variable component) 'none))

(cl-defmethod phony--variable-context (variable (component phony--ast-optional))
  (let ((downstream-context (phony--variable-context
                             variable
                             (phony--ast-children component))))
    (if (eq downstream-context 'none)
        'optional
      downstream-context)))

(cl-defmethod phony--variable-context (variable (component phony--repeat-component))
  (if-let ((downstream-context (phony--variable-context
                                variable
                                (phony--ast-children component))))
      'repeat))

(defun phony--speech-insert-rule (rule)
  (insert (format "<user.%1$s>:\n    user.emacs_lisp(%1$s)\n\n"
                  (phony--rule-talon-name rule))))

(defun phony--to-python-identifier (symbol)
  (concat "phony_" (replace-regexp-in-string (rx (not alnum)) "_" (symbol-name symbol))))

(cl-defgeneric phony--speech-insert-python (rule))

(cl-defmethod phony--speech-insert-python ((rule phony--open-rule))
  ;; Do not insert rule if there are no alternatives
  (when (phony--open-rule-alternatives rule)
    (insert "@module.capture(rule=\n        '  "
            (string-join (seq-map
                          (lambda (alternative)
                            (format "<user.%s>'"
                                    (phony--rule-talon-name
                                     (gethash alternative phony--rules))))
                          (phony--open-rule-alternatives rule))
                         "\n        '| ")
            ")\n")
    (insert
     (format "def %s(m) -> str:\n    return %s\n"
             (phony--rule-talon-name rule)
             (if (phony--open-rule-transformation rule)
                 (format "f\"(%s {m[0]})\""
                         (phony--open-rule-transformation rule))
                 "m[0]")))))

(cl-defmethod phony--speech-insert-python ((rule phony--procedure-rule))
  (insert "@module.capture(rule='" (phony--rule-talon-pattern rule) "')\n"
          "def " (phony--rule-talon-name rule) "(m) -> str:\n")
  (seq-doseq (argument (phony--procedure-rule-arglist rule))
    (let ((variable
           (phony--find-variable-component
            argument
            (phony--procedure-rule-components rule))))
      (insert "    " (phony--to-python-identifier
                      (phony--ast-variable-argument variable))
              " = ")
      (if (not variable)
          (insert "'nil'")
        (let* ((variable-context
                (phony--variable-context
                 variable
                 (phony--procedure-rule-components rule)))
               (form (phony--ast-variable-form variable))
               (attribute-name
                (phony--ast-talon-name form)))
          (when (eq variable-context 'repeat)
            (setq attribute-name (concat attribute-name "_list")))
          (if (phony--ast-talon-capture-p form)
              (insert (format "from_talon_capture(m.%1$s) if hasattr(m,'%1$s') else 'nil'"
                              attribute-name))
            (pcase variable-context
              ('none
               (insert (format "m.%s" attribute-name)))
              ('optional
               (insert (format "getattr(m,'%s','nil')" attribute-name)))
              ('repeat
               (insert (format "'(list ' + ' '.join(getattr(m,'%s',[])) + ')'" attribute-name)))
              (_ (error "Internal error: Unrecognized context %s" variable-context))))))
      (insert "\n")))
  (insert
   (format "    return f\"(%s %s)\"\n"
           (phony--procedure-rule-function rule)
           (string-join
            (seq-map (lambda (argument)
                       (format "{%s}" (phony--to-python-identifier argument)))
                     (phony--procedure-rule-arglist rule))
            " "))))

(cl-defun phony--export-mode (mode entries)
  (mkdir "~/.talon/user/emacs-gen" t)
  (with-temp-file (format "~/.talon/user/emacs-gen/%s.talon" mode)
    (unless (eq mode 'global)
      (insert (format "user.emacs_mode: /:%s:/\n" mode)))
    (insert "-\n")
    (seq-doseq (rule entries)
      (when (phony--rule-export rule)
        (phony--speech-insert-rule rule)))))

(defun phony--export-all ()
  (let* ((rules (hash-table-values phony--rules))
         (modes (seq-uniq
                 (seq-mapcat #'phony--rule-modes
                            rules))))
    (seq-doseq (mode modes)
      (phony--export-mode
       mode
       (seq-filter (lambda (rule)
                     (seq-contains-p (phony--rule-modes rule) mode))
                   rules)))

    (with-temp-file "~/.talon/user/emacs-gen/rules.py"
      (insert "import talon\n\n"
              "module = talon.Module()\n"
              "context = talon.Context()\n\n"
              ;; I heard you like escapes
              "def escape(str):\n"
              "    return str.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\")"
              "\n\n"
              "def from_talon_capture(capture):\n"
              "    formatted = None\n"
              "    if isinstance(capture,str):\n        formatted = '\"' + escape(capture) + '\"'\n"
              "    elif isinstance(capture,int):\n        formatted = f\"{capture}\"\n"
              "    elif isinstance(capture,talon.grammar.vm.Phrase):\n        formatted = '\"' + escape(\" \".join(capture)) + '\"'\n"
              "    elif isinstance(capture,list):\n        formatted = '(list ' + ' '.join([from_talon_capture(x) for x in capture]) + ')'\n"
              "    else:\n        raise TypeError(f\"Talon capture must have type str, int, list or talon.grammar.vm.Phrase, had type {type(capture)}\")\n"
              "    return formatted\n\n")
      (seq-doseq (command rules)
        (insert "\n")
        (phony--speech-insert-python command)))))

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
                               (export nil))
  (setq arglist (byte-compile-arglist-vars arglist))
  (setq mode (ensure-list mode))
  (phony-remove-rule function)
  (let* ((components
          (seq-map (lambda (component)
                     (phony--parse-speech-component component (byte-compile-arglist-vars arglist)))
                   speech-pattern)))
    (puthash function
             (make-phony--procedure-rule
              :function function
              :components components
              :arglist arglist
              :modes mode
              :talon-name (or talon-name
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

(defun phony--split-arguments-pattern (declaration-args)
  (let ((arguments '()))
    (while (keywordp (car declaration-args))
      (push (seq-take declaration-args 2) arguments)
      (setq declaration-args (seq-drop declaration-args 2)))
    (cons (apply #'append arguments) declaration-args)))

(cl-defun phony--speech-declaration (function arglist &rest declaration-args)
  (let* ((split-args (phony--split-arguments-pattern declaration-args))
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
