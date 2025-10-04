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

(cl-defgeneric phony--ast-match-string (element))

(cl-defmethod phony--ast-match-string ((element phony--element-literal))
  (string-replace "'" "\\'" (phony--element-literal-string element)))

(cl-defmethod phony--ast-match-string ((element phony--element-rule))
  (let ((rule (phony--get-rule (phony--element-rule-name element))))
    (format (if (phony--dictionary-p rule) "{user.%s}" "<user.%s>")
            (phony--external-name
             (phony--get-rule
              (phony--element-rule-name element))))))

(cl-defmethod phony--ast-match-string ((element phony--element-sequence))
  (string-join (seq-map #'phony--ast-match-string
                        (phony--element-sequence-elements element))
               " "))

(cl-defmethod phony--ast-match-string ((element phony--element-optional))
  (format "[%s]" (phony--ast-match-string
                  (phony--element-optional-element element))))

(cl-defmethod phony--ast-match-string ((element phony--element-one-or-more))
  (format "(%s)+" (phony--ast-match-string
                   (phony--element-one-or-more-element element))))

(cl-defmethod phony--ast-match-string ((element phony--element-zero-or-more))
  (format "(%s)*" (phony--ast-match-string
                   (phony--element-zero-or-more-element element))))

(cl-defmethod phony--ast-match-string ((element phony--element-external-rule))
  (format "<%s>" (string-join
                  (seq-map #'symbol-name
                           (append
                            (phony--element-external-rule-namespace element)
                            (list (phony--element-external-rule-name element))))
                  ".")))

(cl-defmethod phony--ast-match-string ((element phony--element-argument))
  (phony--ast-match-string
   (phony--element-argument-form element)))

(cl-defgeneric phony--rule-talon-pattern (rule))

(cl-defmethod phony--rule-talon-pattern ((rule phony--procedure-rule))
  (let* ((elements
          (phony--element-sequence-elements
           (phony--procedure-rule-element rule)))
         (spoken-elements
          (seq-map #'phony--ast-match-string elements)))
    (string-join spoken-elements " ")))

(cl-defmethod phony--rule-talon-pattern ((rule phony--open-rule))
  (string-join (seq-map
                (lambda (alternative)
                  (format "<user.%s>"
                          (phony--rule-external-name
                           (phony--get-rule alternative))))
                (phony--open-rule-alternatives rule))
               " | "))

(cl-defgeneric phony--variable-context (variable elements)
  nil)

(cl-defmethod phony--variable-context (variable (element phony--element-sequence))
  (thread-last
    (phony--element-sequence-elements element)
    (seq-map (lambda (element) (phony--variable-context variable element)))
    (seq-find #'identity)))

(cl-defmethod phony--variable-context (variable (element phony--element-argument))
  (when (eq variable element) 'none))

(cl-defmethod phony--variable-context (variable (element phony--element-optional))
  (let ((downstream-context (phony--variable-context
                             variable
                             (phony--element-optional-element element))))
    (if (eq downstream-context 'none)
        'optional
      downstream-context)))

(cl-defmethod phony--variable-context (variable (element phony--element-repeat))
  (if-let ((downstream-context (phony--variable-context
                                variable
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

(cl-defgeneric phony--speech-insert-python (rule))

(cl-defmethod phony--speech-insert-python ((rule phony--open-rule))
  ;; Do not insert rule if there are no alternatives
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
                 "m[0]")))))

(cl-defmethod phony--speech-insert-python ((rule phony--procedure-rule))
  (insert "@module.capture(rule='" (phony--rule-talon-pattern rule) "')\n"
          "def " (phony--rule-external-name rule) "(m) -> str:\n")
  (seq-doseq (argument (phony--procedure-rule-arglist rule))
    (let ((variable
           (car-safe (phony--collect
                      (lambda (element)
                        (and (phony--element-argument-p element)
                             (eq (phony--element-argument-name element)
                                 argument)))
                      (phony--procedure-rule-element rule)))))
      (insert "    " (phony--to-python-identifier argument)
              " = ")
      (if (not variable)
          (insert "'nil'")
        (let* ((variable-context
                (phony--variable-context
                 variable
                 (phony--procedure-rule-element rule)))
               (match-form (phony--element-argument-form variable))
               (attribute-name (phony-talon--element-name match-form)))
          (when (eq variable-context 'repeat)
            (setq attribute-name (concat attribute-name "_list")))
          (if (phony--element-external-rule-p match-form)
              (insert (format "from_talon_capture(m.%1$s) if hasattr(m,'%1$s') else 'nil'"
                              attribute-name))
            (pcase variable-context
              ('none
               (insert (format "m.%s" attribute-name)))
              ('optional
               (insert (format "getattr(m,'%s','nil')" attribute-name)))
              ('repeat
               (insert (format "'(list ' + ' '.join(getattr(m,'%s',[])) + ')'" attribute-name)))
              (_ (error "Internal error: Unrecognized context %S (argument %S in rule %S)"
                        variable-context
                        variable
                        (phony--rule-name rule)))))))
      (insert "\n")))
  (insert
   (format "    return f\"(%s %s)\"\n"
           (phony--procedure-rule-function rule)
           (string-join
            (seq-map (lambda (argument)
                       (format "{%s}" (phony--to-python-identifier argument)))
                     (phony--procedure-rule-arglist rule))
            " "))))

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
      (seq-doseq (command rules)
        (insert "\n")
        (phony--speech-insert-python command)))

    (let ((link-path (expand-file-name "~/.talon/user/phony-generated-rules")))
      (if (or (not (file-exists-p link-path))
              (file-symlink-p link-path))
          (make-symbolic-link (phony--output-directory "talon")
                              link-path t)
        (warn "Path already exists and is not preexistent symlink: %S" link-path)))))

(provide 'phony-talon)
;;; phony-talon.el ends here
