;;; phony-talon.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience
;; Created: 26 Jul 2025

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

(require 'phony)

(cl-defgeneric phony-talon--element-name (element)
  "Return the Talon capture name for ELEMENT."
  (error "No talon name for element %S" element))

(cl-defmethod phony-talon--element-name ((element phony--element-rule))
  (phony--external-name
   (phony--element-rule-name element)))

(cl-defmethod phony-talon--element-name ((element phony--element-external-rule))
  (phony--element-external-rule-name element))

(cl-defgeneric phony--ast-match-string (element rule)
  "Return the Talon pattern string for matching ELEMENT.

RULE provides context for occurrence numbering.")

(cl-defmethod phony--ast-match-string ((element phony--element-literal) _rule)
  (string-replace "'" "\\'" (phony--element-literal-string element)))

(cl-defmethod phony--ast-match-string ((element phony--element-rule) rule)
  (let* ((occurrence-number (phony-talon--occurrence-number element rule))
         (match-rule (phony--get-rule (phony--element-rule-name element))))
    ;; Avoid capture indirection unless required.
    (if (and (phony--dictionary-p match-rule)
             (eq occurrence-number 0)
             (phony--rule-always-active-p match-rule))
        (format "{user.%s}"
                (phony--external-name match-rule))
      (format "<user.%s%s>"
              (phony--external-name
               (phony--get-rule
                (phony--element-rule-name element)))
              (if (>= occurrence-number 1)
                  (format "_phony_clone%s"
                          (1- occurrence-number))
                "")))))

(cl-defmethod phony--ast-match-string ((element phony--element-sequence) rule)
  (string-join (seq-map (lambda (element) (phony--ast-match-string element rule))
                        (phony--element-sequence-elements element))
               " "))

(cl-defmethod phony--ast-match-string ((element phony--element-optional) rule)
  (format "[%s]" (phony--ast-match-string
                  (phony--element-optional-element element)
                  rule)))

(cl-defmethod phony--ast-match-string ((element phony--element-one-or-more) rule)
  (format "(%s)+" (phony--ast-match-string
                   (phony--element-one-or-more-element element)
                   rule)))

(cl-defmethod phony--ast-match-string ((element phony--element-zero-or-more) rule)
  (format "(%s)*" (phony--ast-match-string
                   (phony--element-zero-or-more-element element)
                   rule)))

(cl-defmethod phony--ast-match-string ((element phony--element-external-rule) _rule)
  (format "<%s>" (string-join
                  (seq-map #'symbol-name
                           (append
                            (phony--element-external-rule-namespace element)
                            (list (phony--element-external-rule-name element))))
                  ".")))

(cl-defmethod phony--ast-match-string ((element phony--element-argument) rule)
  (phony--ast-match-string
   (phony--element-argument-element element)
   rule))

(cl-defgeneric phony--rule-talon-pattern (rule analysis-data)
  "Return the full Talon rule pattern for RULE given ANALYSIS-DATA.")

(cl-defmethod phony--rule-talon-pattern ((rule phony--procedure-rule) _analysis-data)
  (let* ((element (phony--procedure-rule-element rule)))
    (concat "'" (phony--ast-match-string element rule) "'")))

(cl-defmethod phony--rule-talon-pattern ((rule phony--open-rule) analysis-data)
  (if (gethash rule (phony--analysis-data-productions analysis-data))
      (concat
       "\n        '  "
       (string-join (seq-map
                     (lambda (alternative)
                       (format "<user.%s>'"
                               (phony--external-name alternative)))
                     (gethash rule (phony--analysis-data-productions analysis-data)))
                    "\n        '| "))
    "'<user.disabled_phony_rule>'"))

(cl-defmethod phony--rule-talon-pattern ((rule phony--dictionary) _analysis-data)
  (concat "'{user." (phony--rule-external-name rule) "}'"))

(cl-defgeneric phony--argument-context (argument elements)
  "Return the context of ARGUMENT within ELEMENTS.

The context is one of: `none' (singular occurrence), `optional',
or `repeat'.  Returns nil if ARGUMENT is not found in ELEMENTS."
  (ignore argument elements)
  nil)

(cl-defmethod phony--argument-context (argument (element phony--element-sequence))
  (thread-last
    (phony--element-sequence-elements element)
    (seq-map (apply-partially #'phony--argument-context argument))
    (seq-find #'identity)))

(cl-defmethod phony--argument-context (argument (element phony--element-argument))
  (when (eq argument element) 'none))

(cl-defmethod phony--argument-context (argument (element phony--element-optional))
  (let ((downstream-context (phony--argument-context
                             argument
                             (phony--element-optional-element element))))
    (if (eq downstream-context 'none)
        'optional
      downstream-context)))

(cl-defmethod phony--argument-context (argument (element phony--element-repeat))
  (if-let ((downstream-context (phony--argument-context
                                argument
                                (phony--element-repeat-element element))))
      'repeat))

(defun phony--speech-insert-rule (rule)
  "Insert a .talon rule entry for procedure RULE."
  (when (phony--procedure-rule-anchor-beginning-p rule)
    (insert "^"))
  (insert (format "<user.%s>" (phony--rule-external-name rule)))
  (when (phony--procedure-rule-anchor-end-p rule)
    (insert "$"))
  (insert (format ":\n    user.phony_evaluate_emacs_lisp(%s)\n\n"
                  (phony--rule-external-name rule))))

(defun phony-talon--insert-capture-preamble (rule analysis-data)
  "Insert the Python capture preamble for RULE."
  (if (phony--rule-always-active-p rule)
      (insert "@module.capture(rule=" (phony--rule-talon-pattern rule analysis-data) ")\n")
    (insert "# Base case for when the rule is inactive\n")
    (phony-talon--insert-python-disabled rule)
    (insert "\ncontext = talon.Context()\n")
    (insert "context.matches = \"\"\"\ntag: user." (phony--rule-external-name rule) "\n\"\"\"\n")
    (insert "@context.capture('user." (phony--rule-external-name rule)
            "', rule=" (phony--rule-talon-pattern rule analysis-data) ")\n")))

(cl-defgeneric phony-talon--insert-python (rule))

(cl-defmethod phony-talon--insert-python ((rule phony--open-rule))
  (insert
   (format "def %s(m) -> str:\n    return %s\n"
           (phony--rule-external-name rule)
           "m[0]")))

(defun phony-talon--find-argument-element (argument rule)
  "Find element matching ARGUMENT in RULE.
Return nil if no such element exists."
  (car-safe
   (phony--collect
    (lambda (element)
      (and (phony--element-argument-p element)
           (eq (phony--element-argument-name element)
               argument)))
    (phony--procedure-rule-element rule))))

(defun phony-talon--occurrence-number (match-element rule)
  "Get index of occurence of MATCH-ELEMENT in procedure rule RULE.

This goes through all match elements of RULE and returns the index of
MATCH-ELEMENT among all of those `equal' to it."
  (seq-position
   (phony--collect
    (apply-partially #'equal match-element)
    (phony--procedure-rule-element rule))
   match-element
   #'eq))

(cl-defmethod phony-talon--insert-python ((rule phony--procedure-rule))
  (insert "def " (phony--rule-external-name rule) "(m) -> str:\n")
  (seq-doseq (argument (phony--procedure-rule-arglist rule))
    (let* ((argument-element
            (phony-talon--find-argument-element argument rule)))
      (insert "    " (phony--to-python-identifier argument)
              " = ")
      (if (not argument-element)
          (insert "'nil'")
        (let* ((match-form (phony--element-argument-element argument-element))
               (rule-occurrence-number
                (phony-talon--occurrence-number match-form rule))
               (argument-context
                (phony--argument-context
                 argument-element
                 (phony--procedure-rule-element rule)))
               (attribute-name (phony-talon--element-name match-form)))
          (when (>= rule-occurrence-number 1)
            (setq attribute-name (concat attribute-name
                                         (format "_phony_clone%s"
                                                 (1- rule-occurrence-number)))))
          (when (eq argument-context 'repeat)
            (setq attribute-name (concat attribute-name "_list")))
          (if (phony--element-external-rule-p match-form)
              (insert (format "from_talon_capture(m.%1$s) if hasattr(m,'%1$s') else 'nil'"
                              attribute-name))
            (pcase argument-context
              ('none
               (insert (format "m.%s" attribute-name)))
              ('optional
               (insert (format "getattr(m,'%s','nil')" attribute-name)))
              ('repeat
               (insert (format "'(list ' + ' '.join(getattr(m,'%s',[])) + ')'" attribute-name)))
              (_ (error "Internal error: Unrecognized context %S (argument %S in rule %S)"
                        argument-context
                        argument-element
                        (phony--rule-name rule)))))))
      (insert "\n")))
  (insert
   (format "    return f\"(%s %s)\"\n"
           (symbol-name (phony--procedure-rule-name rule))
           (string-join
            (seq-map (lambda (argument)
                       (format "{%s}" (phony--to-python-identifier argument)))
                     (phony--procedure-rule-arglist rule))
            " "))))

(cl-defmethod phony-talon--insert-python ((rule phony--dictionary))
  (insert "def " (phony--rule-external-name rule) "(m) -> str:\n"
          "    return m[0]"))

(defun phony-talon--insert-python-disabled (rule)
  "Insert a Python stub for disabled RULE that returns False."
  (insert "# Disabled\n"
          "@module.capture(rule='<user.disabled_phony_rule>')\n"
          "def " (phony--rule-external-name rule) "(m) -> str:\n"
          "    return False\n"))

(defvar phony-talon--always-listen nil
  "Whether to always listen for utterances, even if Emacs is not focused.

If you are using EXWM, you probably want this to be t.")

(defun phony-talon--clone-rule (rule times)
  "Insert TIMES clone captures for RULE.

Clone captures are needed because Talon does not differentiate
between multiple captures of the same name in one pattern."
  (dotimes (i times)
    (insert (format "\n@module.capture(rule='<user.%s>')\n"
                    (phony--rule-external-name rule)))
    (insert
     (format "def %s_phony_clone%s(m) -> str:\n    return m[0]\n"
             (phony--rule-external-name rule) i))))

(cl-defun phony-talon--insert-exported-rules (rules)
  "Insert .talon entries for all exported procedure rules in RULES."
  (unless phony-talon--always-listen (insert "app: emacs\n"))
  (insert "-\n")
  (seq-doseq (rule rules)
    (when (and
           (phony--procedure-rule-p rule)
           (phony--procedure-rule-interactive-p rule))
      (phony--speech-insert-rule rule))))

(defun phony-talon-export (analysis-data)
  "Generate all Talon files from the current phony grammar.

This function creates .talon and .py files in the output directory and
symlinks them into ~/.talon/user/."
  (mkdir (phony--output-directory "talon") t)
  (let ((rules (phony--get-rules)))
    (with-temp-file (phony--output-directory "talon" "exported-rules.talon")
      (phony-talon--insert-exported-rules rules))

    (with-temp-file (phony--output-directory
                     "talon"
                     "read_dictionaries.py")
      (insert "from talon import Context, Module\n"
              "import talon, json, os.path\n"
              "\n"
              "module = Module()\n"
              "context = Context()\n"
              "\n"
              "def load_lists(path, dummy_argument):\n"
              "    with open(path, 'r') as inn:\n"
              "        message = json.load(inn)\n"
              "        for list_name in message:\n"
              "            module.list(list_name, '')\n"
              "            context.lists['user.' + list_name] = message[list_name]\n"
              "\n"
              "path = '" (phony--output-directory "dictionaries.json") "'\n"
              "if os.path.isfile(path):\n"
              "    load_lists(path, None)\n"
              "talon.fs.watch(path, load_lists)\n"))

    (with-temp-file (phony--output-directory
                     "talon"
                     "evaluate_elisp.py")
      (insert "from talon import Module\n"
              "import subprocess\n"
              "\n"
              "module = Module()\n"
              "\n"
              "def evaluate_lisp_async(expression: str):\n"
              "    return subprocess.Popen(\n"
              "        ['emacsclient',\n"
              "         '--eval',\n"
              "         f\"(phony--evaluate '{expression})\"])\n"
              "\n"
              "def evaluate_lisp(expression: str):\n"
              "    evaluate_lisp_async(expression).wait()\n"
              "\n"
              "@module.action_class\n"
              "class Actions:\n"
              "    def phony_evaluate_emacs_lisp(expression: str):\n"
              "        'Evaluate an elisp expression'\n"
              "        evaluate_lisp(expression)\n"))

    (with-temp-file (phony--output-directory "talon" "rules.py")
      (insert "import talon\n\n"
              "module = talon.Module()\n"
              ;; I heard you like escapes
              "def quote_string(str):\n"
              "    return '\"' + str.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\") + '\"'\n"
              "\n"
              "def from_talon_capture(capture):\n"
              "    formatted = None\n"
              "    if isinstance(capture,str):\n"
              "        formatted = quote_string(capture)\n"
              "    elif isinstance(capture,int):\n"
              "        formatted = str(capture)\n"
              "    elif isinstance(capture,talon.grammar.vm.Phrase):\n"
              "        formatted = quote_string(' '.join(capture))\n"
              "    elif isinstance(capture,list):\n"
              "        formatted = '(list ' + ' '.join([from_talon_capture(x) for x in capture]) + ')'\n"
              "    else:\n"
              "        raise TypeError(f\"Talon capture must have type str, int, list or talon.grammar.vm.Phrase, had type {type(capture)}\")\n"
              "    return formatted\n"
              "\n"
              "tag_context = talon.Context()\n"
              "\n"
              "def update_active_tags():\n"
              "    with open('" (phony--output-directory "active-rules") "', 'r') as inn:\n"
              "        tags = {'user.' + tag for tag in inn.read().strip().split('\\n')}\n"
              "        if tags != tag_context.tags:"
              "            tag_context.tags = tags\n"
              "\n"
              "talon.fs.watch('" (phony--output-directory "active-rules") "',\n"
              "        lambda x, y: update_active_tags())\n")
      (seq-doseq (rule rules)
        (insert "\n")
        (unless (phony--rule-always-active-p rule)
          (insert (format "module.tag('%1$s', desc='Enabled when %1$s should be enabled')\n"
                          (phony--external-name rule))))
        (phony-talon--insert-capture-preamble rule analysis-data)
        (phony-talon--insert-python rule)
        (phony-talon--clone-rule
         rule
         (max 0 (1- (phony--maximum-producer-multiplicity analysis-data rule))))))

    (let ((link-path (expand-file-name "~/.talon/user/phony-generated-rules")))
      (if (or (not (file-exists-p link-path))
              (file-symlink-p link-path))
          (make-symbolic-link (phony--output-directory "talon")
                              link-path t)
        (warn "Path already exists and is not preexistent symlink: %S" link-path)))))

(let ((phony--deny-export-requests-p t))
  ;; Empty lists create no matches yet emit no errors.
  (phony-define-dictionary phony-talon--disabled-rule
    :external-name "disabled_phony_rule"
    '()))

(provide 'phony-talon)
;;; phony-talon.el ends here
