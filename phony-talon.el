;;; phony-talon.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
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

;; This file defines the Talon backend.

;;; Code:

(require 'phony-core)

(defun phony-talon--rule-pattern-resolved (pattern)
  "Return the rule referred to by rule pattern PATTERN."
  ;; TODO: Move this to phony-core, it is a generally useful function.
  (phony--resolved-rule
   (phony--get-rule (phony-pattern--rule-name pattern)
                    (phony--containing-grammar pattern))))

(cl-defgeneric phony-talon--pattern-match-string (pattern)
  "Return the Talon pattern string for matching PATTERN.")

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--literal))
  "Return the Talon pattern string for matching PATTERN."
  (string-replace "'" "\\'" (phony-pattern--literal-string pattern)))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--rule))
  "Return the Talon pattern string for matching PATTERN."
  (format "<user.%s>"
          (phony--rule-external-name
           (phony-talon--rule-pattern-resolved pattern))))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--sequence))
  "Return the Talon pattern string for matching PATTERN."
  (string-join (seq-map #'phony-talon--pattern-match-string
                        (phony-pattern--sequence-patterns pattern))
               " "))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--alternative))
  "Return the Talon pattern string for matching PATTERN."
  (format "(%s)"
          (string-join (seq-map #'phony-talon--pattern-match-string
                                (phony-pattern--alternative-patterns pattern))
                       " | ")))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--optional))
  "Return the Talon pattern string for matching PATTERN."
  (format "[%s]" (phony-talon--pattern-match-string
                  (phony-pattern--optional-pattern pattern))))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--one-or-more))
  "Return the Talon pattern string for matching PATTERN."
  (format "(%s)+" (phony-talon--pattern-match-string
                   (phony-pattern--one-or-more-pattern pattern))))

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--zero-or-more))
  "Return the Talon pattern string for matching PATTERN."
  (format "(%s)*" (phony-talon--pattern-match-string
                   (phony-pattern--zero-or-more-pattern pattern))))

(cl-defmethod phony-talon--pattern-match-string ((_pattern phony-pattern--builtin-word))
  "Return the Talon pattern string for matching PATTERN."
  "<word>")

(cl-defmethod phony-talon--pattern-match-string ((_pattern phony-pattern--builtin-phrase))
  "Return the Talon pattern string for matching PATTERN."
  "<phrase>")

(cl-defmethod phony-talon--pattern-match-string ((pattern phony-pattern--argument))
  "Return the Talon pattern string for matching PATTERN."
  (phony-talon--pattern-match-string
   (phony-pattern--argument-pattern pattern)))

(cl-defgeneric phony-talon--rule-match-string (rule)
  "Return the full Talon rule pattern for RULE.")

(cl-defmethod phony-talon--rule-match-string ((rule phony--procedure-rule))
  "Return the full Talon rule pattern for RULE."
  (let* ((pattern (phony--procedure-rule-pattern rule)))
    (concat "'" (phony-talon--pattern-match-string pattern) "'")))

(cl-defmethod phony-talon--rule-match-string ((rule phony--open-rule))
  "Return the full Talon rule pattern for RULE."
  (if-let* ((alternatives (phony--open-rule-effective-alternatives rule)))
      (concat
       "\n        '  "
       (string-join (seq-map
                     (lambda (alternative)
                       (format "<user.%s>'"
                               (phony--rule-external-name
                                (phony--resolved-rule
                                 (phony--get-rule
                                  alternative (phony--containing-grammar rule))))))
                     alternatives)
                    "\n        '| "))
    "'{user.phony_disabled_rule}'"))

(cl-defmethod phony-talon--rule-match-string ((rule phony--dictionary))
  "Return the full Talon rule pattern for RULE."
  (concat "'{user." (phony--rule-external-name rule) "}'"))

(defun phony-talon--speech-insert-rule (rule)
  "Insert a .talon rule entry for procedure RULE."
  (when (phony--procedure-rule-anchor-beginning-p rule)
    (insert "^"))
  (insert (format "<user.%s>" (phony--rule-external-name rule)))
  (when (phony--procedure-rule-anchor-end-p rule)
    (insert "$"))
  (insert (format ":\n    user.phony_evaluate_emacs_lisp(%s)\n\n"
                  (phony--rule-external-name rule))))

(defun phony-talon--insert-capture-preamble (rule)
  "Insert the Python capture preamble for RULE."
  (if (phony--rule-always-active-p rule)
      (insert "@module.capture(rule=" (phony-talon--rule-match-string rule) ")\n")
    (insert "# Base case for when the rule is inactive\n")
    (phony-talon--insert-python-disabled rule)
    (insert "\ncontext = talon.Context()\n")
    (insert "context.matches = \"\"\"\ntag: user." (phony--rule-external-name rule) "\n\"\"\"\n")
    (insert "@context.capture('user." (phony--rule-external-name rule)
            "', rule=" (phony-talon--rule-match-string rule) ")\n")))

(cl-defgeneric phony-talon--insert-python (rule)
  "Insert python code for capturing RULE.")

(cl-defmethod phony-talon--insert-python ((rule phony--open-rule))
  "Insert python code for capturing RULE."
  (insert
   (format "def %s(m) -> str:\n    return %s\n"
           (phony--rule-external-name rule)
           "m[0]")))

(cl-defmethod phony-talon--insert-python ((rule phony--procedure-rule))
  "Insert python code for capturing RULE."
  (insert "def " (phony--rule-external-name rule) "(m) -> str:\n"
          "     return ' '.join(str(x) for x in m)\n"))

(cl-defmethod phony-talon--insert-python ((rule phony--dictionary))
  "Insert python code for capturing RULE."
  (insert "def " (phony--rule-external-name rule) "(m) -> str:\n"
          "    return m[0]"))

(defun phony-talon--insert-python-disabled (rule)
  "Insert a Python stub for disabled RULE returning False."
  (insert "# Disabled\n"
          "@module.capture(rule='{user.phony_disabled_rule}')\n"
          "def " (phony--rule-external-name rule) "(m) -> str:\n"
          "    return False\n"))

(defvar phony-talon--always-listen nil
  "Whether to always listen for utterances, even if Emacs is not focused.

If you are using EXWM, you probably want this to be t.")

(cl-defun phony-talon--insert-exported-rules (rules)
  "Insert .talon entries for all exported procedure rules in RULES."
  (unless phony-talon--always-listen (insert "app: emacs\n"))
  (insert "-\n")
  (seq-doseq (rule rules)
    (when (and
           (phony--procedure-rule-p rule)
           (phony--procedure-rule-interactive-p rule))
      (phony-talon--speech-insert-rule rule))))

(defun phony-talon--output-path (&rest components)
  "Return output path for talon files.

COMPONENTS are appended as components to the returned path."
  (apply #'phony--output-directory "talon" components))

(defun phony-talon--generate-configuration (grammar)
  "Generate all Talon files from GRAMMAR.

This function creates .talon and .py files in the output directory."
  (mkdir (phony-talon--output-path) t)
  (let* ((rules (phony--get-rules grammar)))
    (with-temp-file (phony-talon--output-path "exported-rules.talon")
      (phony-talon--insert-exported-rules rules))

    (with-temp-file (phony-talon--output-path
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
              "            context.lists['user.' + list_name] = {utterance: utterance for utterance in message[list_name]}\n"
              "\n"
              "path = '" (phony--output-directory "dictionaries.json") "'\n"
              "if os.path.isfile(path):\n"
              "    load_lists(path, None)\n"
              "talon.fs.watch(path, load_lists)\n"))

    (with-temp-file (phony-talon--output-path
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
              "         f\"(phony-evaluate-as-event \\\"{expression}\\\")\"])\n"
              "\n"
              "def evaluate_lisp(expression: str):\n"
              "    evaluate_lisp_async(expression).wait()\n"
              "\n"
              "@module.action_class\n"
              "class Actions:\n"
              "    def phony_evaluate_emacs_lisp(expression: str):\n"
              "        'Evaluate an elisp expression'\n"
              "        evaluate_lisp(expression)\n"))

    (with-temp-file (phony-talon--output-path "rules.py")
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
              ;; We split without providing delimiter, otherwise
              ;; splitting the empty string will yield one empty tag.
              "        tags = {'user.' + tag for tag in inn.read().strip().split()}\n"
              "        if tags != tag_context.tags:\n"
              "            tag_context.tags = tags\n"
              "\n"
              "talon.fs.watch('" (phony--output-directory "active-rules") "',\n"
              "        lambda x, y: update_active_tags())\n"
              "\n"
              "module.list(\"phony_disabled_rule\", desc=\"This cannot match\")\n")
      (seq-doseq (rule (seq-remove #'phony--interface-rule-p rules))
        (insert "\n")
        (unless (phony--rule-always-active-p rule)
          (insert (format "module.tag('%1$s', desc='Enabled when %1$s should be enabled')\n"
                          (phony--rule-external-name rule))))
        (phony-talon--insert-capture-preamble rule)
        (phony-talon--insert-python rule)))))

(defun phony-talon-export (grammar)
  "Generate Talon configuration for detecting utterances of GRAMMAR.

This function creates .talon and .py files in the output directory and
symlinks them into ~/.talon/user/."
  (phony-talon--generate-configuration grammar)
  (let ((link-path (expand-file-name "~/.talon/user/phony-generated-rules")))
    (if (or (not (file-exists-p link-path))
            (file-symlink-p link-path))
        (make-symbolic-link (phony-talon--output-path)
                            link-path t)
      (warn "Path already exists and is not preexistent symlink: %S" link-path))))

(provide 'phony-talon)
;;; phony-talon.el ends here
