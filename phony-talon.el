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

(require 'phony)

(defvar phony-talon-rule-directory nil)

(cl-defgeneric phony--ast-talon-name (element)
  (error "No talon name for element %S" element))

(cl-defmethod phony--ast-talon-name ((element phony--element-rule))
  (phony--rule-external-name
   (phony--get-rule
    (phony--element-rule-name element))))

(cl-defmethod phony--ast-talon-name ((element phony--element-external-rule))
  (phony--element-external-rule-name element))

(cl-defmethod phony--ast-talon-name ((element phony--element-dictionary))
  (phony--dictionary-external-name
   (phony--element-dictionary-name element)))

(cl-defgeneric phony--ast-match-string (element))

(cl-defmethod phony--ast-match-string ((element phony--element-literal))
  (phony--element-literal-string element))

(cl-defmethod phony--ast-match-string ((element phony--element-dictionary))
  (format "{user.%s}" (phony--dictionary-external-name
                       (phony--element-dictionary-name element))))

(cl-defmethod phony--ast-match-string ((element phony--element-optional))
  (format "[%s]" (phony--ast-match-string
                  (phony--ast-children element))))

(cl-defmethod phony--ast-match-string ((element phony--element-one-or-more))
  (format "(%s)+" (phony--ast-match-string
                   (phony--ast-children element))))

(cl-defmethod phony--ast-match-string ((element phony--element-zero-or-more))
  (format "(%s)*" (phony--ast-match-string
                   (phony--ast-children element))))

(cl-defmethod phony--ast-match-string ((element phony--element-external-rule))
  (format "<%s>" (string-join
                  (seq-map #'symbol-name
                           (append
                            (phony--element-external-rule-namespace element)
                            (list (phony--element-external-rule-name element))))
                  ".")))

(cl-defmethod phony--ast-match-string ((element phony--element-rule))
  (format "<user.%s>"
          (phony--rule-external-name
           (phony--get-rule
            (phony--element-rule-name element)))))

(cl-defmethod phony--ast-match-string ((element phony--element-argument))
  (phony--ast-match-string
   (phony--element-argument-form element)))

(cl-defmethod phony--ast-match-string ((element-list list))
  (string-join (seq-map #'phony--ast-match-string
                        element-list)
               " "))

(cl-defgeneric phony--rule-talon-pattern (rule))

(cl-defmethod phony--rule-talon-pattern ((rule phony--procedure-rule))
  (let* ((elements
          (phony--procedure-rule-elements rule))
         (spoken-elements
          (seq-map #'phony--ast-match-string elements)))
    (string-join spoken-elements " ")))

(cl-defmethod phony--rule-talon-pattern ((rule phony--open-rule))
  (string-join (seq-map
                (lambda (alternative)
                  (format "<user.%s>"
                          (phony--rule-external-name
                           (gethash alternative phony--rules))))
                (phony--open-rule-alternatives rule))
               " | "))

(cl-defgeneric phony--variable-context (variable elements)
  nil)

(cl-defmethod phony--variable-context (variable (elements list))
  (thread-last
    elements
    (seq-map (lambda (element) (phony--variable-context variable element)))
    (seq-find #'identity)))

(cl-defmethod phony--variable-context (variable (element phony--element-argument))
  (when (eq variable element) 'none))

(cl-defmethod phony--variable-context (variable (element phony--element-optional))
  (let ((downstream-context (phony--variable-context
                             variable
                             (phony--ast-children element))))
    (if (eq downstream-context 'none)
        'optional
      downstream-context)))

(cl-defmethod phony--variable-context (variable (element phony--element-repeat))
  (if-let ((downstream-context (phony--variable-context
                                variable
                                (phony--ast-children element))))
      'repeat))

(defun phony--speech-insert-rule (rule)
  (insert (format "<user.%1$s>:\n    user.emacs_lisp(%1$s)\n\n"
                  (phony--rule-external-name rule))))

(cl-defgeneric phony--speech-insert-python (rule))

(cl-defmethod phony--speech-insert-python ((rule phony--open-rule))
  ;; Do not insert rule if there are no alternatives
  (when (phony--open-rule-alternatives rule)
    (insert "@module.capture(rule=\n        '  "
            (string-join (seq-map
                          (lambda (alternative)
                            (format "<user.%s>'"
                                    (phony--rule-external-name
                                     (gethash alternative phony--rules))))
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
           (phony--find-variable-element
            argument
            (phony--procedure-rule-elements rule))))
      (insert "    " (phony--to-python-identifier
                      (phony--element-argument-name variable))
              " = ")
      (if (not variable)
          (insert "'nil'")
        (let* ((variable-context
                (phony--variable-context
                 variable
                 (phony--procedure-rule-elements rule)))
               (form (phony--element-argument-form variable))
               (attribute-name
                (phony--ast-talon-name form)))
          (when (eq variable-context 'repeat)
            (setq attribute-name (concat attribute-name "_list")))
          (if (phony--element-external-rule-p form)
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

(cl-defun phony-talon--export-mode (mode entries)
  (mkdir "~/.talon/user/emacs-gen" t)
  (with-temp-file (file-name-concat phony-talon-rule-directory (format "%s.talon" mode))
    (unless (eq mode 'global)
      (insert (format "user.emacs_mode: /:%s:/\n" mode)))
    (insert "-\n")
    (seq-doseq (rule entries)
      (when (phony--rule-export rule)
        (phony--speech-insert-rule rule)))))

(defun phony-talon-export ()
  (let* ((rules (hash-table-values phony--rules))
         (modes (seq-uniq
                 (seq-mapcat #'phony--rule-modes
                             rules))))
    (seq-doseq (mode modes)
      (phony-talon--export-mode
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

(provide 'phony-talon)
;;; phony-talon.el ends here
