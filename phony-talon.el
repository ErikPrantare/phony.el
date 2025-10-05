;;; phony-talon.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience
;; Created: 26 Jul 2025

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(cl-defgeneric phony-talon--element-name (element)
  (error "No talon name for element %S" element))

(cl-defmethod phony-talon--element-name ((element phony--element-rule))
  (phony--external-name
   (phony--element-rule-name element)))

(cl-defmethod phony-talon--element-name ((element phony--element-external-rule))
  (phony--element-external-rule-name element))

(cl-defgeneric phony--ast-match-string (element) rule)

(cl-defmethod phony--ast-match-string ((element phony--element-literal) rule)
  (string-replace "'" "\\'" (phony--element-literal-string element)))

(cl-defmethod phony--ast-match-string ((element phony--element-rule) rule)
  (let* ((occurrence-number (phony-talon--occurrence-number element rule))
         (match-rule (phony--get-rule (phony--element-rule-name element))))
    (cond
     ((not (phony--dictionary-p match-rule))
      (format "<user.%s%s>"
              (phony--external-name
               (phony--get-rule
                (phony--element-rule-name element)))
              (if (>= occurrence-number 1)
                  (format "_phony_clone%s"
                          (1- occurrence-number))
                "")))
     ;; TODO: Also handle multiple occurences of dictionaries
     (t (format "{user.%s}"
                (phony--external-name
                 (phony--get-rule
                  (phony--element-rule-name element))))))))

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

(cl-defmethod phony--ast-match-string ((element phony--element-external-rule) rule)
  (format "<%s>" (string-join
                  (seq-map #'symbol-name
                           (append
                            (phony--element-external-rule-namespace element)
                            (list (phony--element-external-rule-name element))))
                  ".")))

(cl-defmethod phony--ast-match-string ((element phony--element-argument) rule)
  (phony--ast-match-string
   (phony--element-argument-form element)
   rule))

(cl-defgeneric phony--rule-talon-pattern (rule))

(cl-defmethod phony--rule-talon-pattern ((rule phony--procedure-rule))
  (let* ((element (phony--procedure-rule-element rule)))
    (phony--ast-match-string element rule)))

(cl-defmethod phony--rule-talon-pattern ((rule phony--open-rule))
  (string-join (seq-map
                (lambda (alternative)
                  (format "<user.%s>"
                          (phony--rule-external-name
                           (phony--get-rule alternative))))
                (phony--open-rule-alternatives rule))
               " | "))

(cl-defgeneric phony--argument-context (argument elements)
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
  (when (phony--procedure-rule-anchor-beginning-p rule)
    (insert "^"))
  (insert (format "<user.%s>" (phony--rule-external-name rule)))
  (when (phony--procedure-rule-anchor-end-p rule)
    (insert "$"))
  (insert (format ":\n    user.emacs_lisp(%s)\n\n"
                  (phony--rule-external-name rule))))

(cl-defgeneric phony--speech-insert-python (rule clone-amount))

(defun phony-talon--clone-rule (rule times)
  (dotimes (i times)
    (insert (format "\n@module.capture(rule='<user.%s>')\n"
                    (phony--rule-external-name rule)))
    (insert
     (format "def %s_phony_clone%s(m) -> str:\n    return m[0]\n"
             (phony--rule-external-name rule) i))))

(cl-defmethod phony--speech-insert-python ((rule phony--open-rule) clone-amount)
  ;; Do not insert rule if there are no alternatives.  This filtering
  ;; should probably be done during analysis, not here.
  (when (phony--open-rule-alternatives rule)
    (insert "@module.capture(rule=\n        '  "
            (string-join (seq-map
                          (lambda (alternative)
                            (format "<user.%s>'"
                                    (phony--external-name alternative)))
                          (phony--open-rule-alternatives rule))
                         "\n        '| ")
            ")\n")
    (insert
     (format "def %s(m) -> str:\n    return %s\n"
             (phony--rule-external-name rule)
             (if (phony--open-rule-transformation rule)
                 (format "f\"(%s {m[0]})\""
                         (phony--open-rule-transformation rule))
               "m[0]")))

    (phony-talon--clone-rule rule clone-amount)))

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

(cl-defmethod phony--speech-insert-python ((rule phony--procedure-rule) clone-amount)
  (insert "@module.capture(rule='" (phony--rule-talon-pattern rule) "')\n"
          "def " (phony--rule-external-name rule) "(m) -> str:\n")
  (seq-doseq (argument (phony--procedure-rule-arglist rule))
    (let* ((argument-element
            (phony-talon--find-argument-element argument rule)))
      (insert "    " (phony--to-python-identifier argument)
              " = ")
      (if (not argument-element)
          (insert "'nil'")
        (let* ((match-form (phony--element-argument-form argument-element))
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
           (phony--procedure-rule-function rule)
           (string-join
            (seq-map (lambda (argument)
                       (format "{%s}" (phony--to-python-identifier argument)))
                     (phony--procedure-rule-arglist rule))
            " ")))

  (phony-talon--clone-rule rule clone-amount))

(cl-defun phony-talon--export-mode (mode entries)
  (with-temp-file (phony--output-directory "talon"
                                           (format "%s.talon" mode))
    (unless (eq mode 'global)
      (insert (format "user.emacs_mode: /:%s:/\n" mode)))
    (insert "-\n")
    (seq-doseq (rule entries)
      (when (and
             (phony--procedure-rule-p rule)
             (phony--procedure-rule-export rule))
        (phony--speech-insert-rule rule)))))

(defun phony-talon-export (analysis-data)
  (mkdir (phony--output-directory "talon") t)
  (let* (;; Handle dictionaries here as well?
         (rules (seq-remove #'phony--dictionary-p (phony--get-rules)))
         (modes (seq-uniq
                 (seq-mapcat (lambda (rule)
                               (when (phony--procedure-rule-p rule)
                                 (phony--procedure-rule-modes rule)))
                             rules))))
    (seq-doseq (mode modes)
      (phony-talon--export-mode
       mode
       (seq-filter
        (lambda (rule)
          (and (phony--procedure-rule-p rule)
               (seq-contains-p (phony--procedure-rule-modes rule) mode)))
        rules)))

    (with-temp-file (phony--output-directory
                     "talon"
                     "read_dictionaries.py")
      (insert (format
               "from talon import Context, Module
import talon, json, os.path

module = Module()
context = Context()

def load_lists(path, dummy_argument):
    with open(path, 'r') as inn:
        message = json.load(inn)
        for list_name in message:
            module.list(list_name, '')
            context.lists['user.' + list_name] = message[list_name]
            print('Loaded list ' + list_name)

path = '%s'
if os.path.isfile(path):
    load_lists(path, None)
talon.fs.watch(path, load_lists)
"
               (phony--output-directory "dictionaries.json"))))

    (with-temp-file (phony--output-directory "talon" "rules.py")
      (insert "import talon\n\n"
              "module = talon.Module()\n"
              "context = talon.Context()\n\n"
              ;; I heard you like escapes
              "def quote_string(str):\n"
              "    return '\"' + str.replace(\"\\\\\", \"\\\\\\\\\").replace(\"\\\"\", \"\\\\\\\"\") + '\"'"
              "\n\n"
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
              "    return formatted\n\n")
      (seq-doseq (rule rules)
        (insert "\n")
        (phony--speech-insert-python
         rule
         (max 0 (1- (phony--maximum-backward-multiplicity analysis-data rule))))))

    (let ((link-path (expand-file-name "~/.talon/user/phony-generated-rules")))
      (if (or (not (file-exists-p link-path))
              (file-symlink-p link-path))
          (make-symbolic-link (phony--output-directory "talon")
                              link-path t)
        (warn "Path already exists and is not preexistent symlink: %S" link-path)))))

(provide 'phony-talon)
;;; phony-talon.el ends here
