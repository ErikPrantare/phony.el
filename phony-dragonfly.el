;;; phony-dragonfly.el ---                           -*- lexical-binding: t; -*-

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

(cl-defgeneric phony-dragonfly--serialize-pattern (pattern)
  (message "%s" pattern)
  `((type . "undefined")))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--ast-literal))
  `((type . "literal")
    (utterance . ,(phony--ast-literal-string literal))))

(cl-defmethod phony-dragonfly--serialize-pattern ((variable phony--ast-variable))
  `((type . "argument")
    (name . ,(symbol-name
              (phony--ast-variable-argument variable)))
    (rule . ,(phony-dragonfly--serialize-pattern
              (phony--ast-variable-form variable)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((dictionary phony--ast-element))
  `((type . "dictionary")
    (name . ,(symbol-name
              (phony--dictionary-external-name
               (phony--ast-element-list dictionary))))))

(cl-defmethod phony-dragonfly--serialize-pattern ((pattern list))
  `((type . "sequence")
    (elements . ,(seq-into (seq-map #'phony-dragonfly--serialize-pattern pattern)
                           'vector))))

(cl-defgeneric phony-dragonfly--serialize-rule-concrete (rule))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--procedure-rule))
  `((type . "procedure-definition")
    (name . ,(phony--procedure-rule-external-name rule))
    (function . ,(symbol-name (phony--procedure-rule-function rule)))
    (pattern . ,(phony-dragonfly--serialize-pattern
                 (phony--procedure-rule-components rule)))
    (argument-list . ,(seq-into
                       (seq-map #'symbol-name
                                (phony--procedure-rule-arglist rule))
                       'vector))
    (export . ,(if (phony--procedure-rule-export rule) t :false))))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--open-rule))
  `((type . "open")
    (name . ,(phony--rule-external-name rule))))

(defun phony-dragonfly--serialize-rule (rule)
  `((,(make-symbol (phony--rule-external-name rule)))
    . (phony-dragonfly--serialize-rule-concrete rule)))

(defun phony-dragonfly--serialize-rules ()
  (seq-map #'phony-dragonfly--serialize-rule (hash-table-values phony--rules)))

(defun phony-dragonfly-export ()
  (interactive)
  (with-temp-file "~/temp/rules.json"
    (json-insert (phony-dragonfly--serialize-rules))
    (json-pretty-print-buffer)))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
