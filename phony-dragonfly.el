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

(cl-defgeneric phony-dragonfly--serialize-pattern (pattern)
  `((type . "undefined")))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-literal))
  `((type . "literal")
    (utterance . ,(phony--element-literal-string literal))))

(cl-defmethod phony-dragonfly--serialize-pattern ((variable phony--element-argument))
  `((type . "argument")
    (name . ,(symbol-name
              (phony--element-argument-name variable)))
    (rule . ,(phony-dragonfly--serialize-pattern
              (phony--element-argument-form variable)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((dictionary phony--element-rule))
  `((type . "rule")
    (name . ,(phony--external-name
              (phony--element-rule-name dictionary)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-one-or-more))
  `((type . "one-or-more")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-zero-or-more))
  `((type . "zero-or-more")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-optional))
  `((type . "optional")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-external-rule))
  `((type . "impossible")))

(cl-defmethod phony-dragonfly--serialize-pattern ((pattern list))
  `((type . "sequence")
    (elements . ,(seq-into (seq-map #'phony-dragonfly--serialize-pattern pattern)
                           'vector))))

(cl-defgeneric phony-dragonfly--serialize-rule-concrete (rule))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--procedure-rule))
  `((type . "procedure-definition")
    (name . ,(phony--external-name rule))
    (function . ,(symbol-name (phony--procedure-rule-function rule)))
    (pattern . ,(phony-dragonfly--serialize-pattern
                 (phony--procedure-rule-elements rule)))
    (argument-list . ,(seq-into
                       (seq-map #'symbol-name
                                (phony--procedure-rule-arglist rule))
                       'vector))
    (export . ,(if (phony--procedure-rule-export rule) t :false))))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--open-rule))
  `((type . "open")
    (name . ,(phony--external-name rule))
    (alternatives . ,(seq-into (seq-map #'phony--external-name
                                        (phony--open-rule-alternatives rule))
                               'vector))))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--dictionary))
  `((type . "dictionary")
    (name . ,(phony--external-name rule))))

(defun phony-dragonfly--serialize-rule (rule)
  `(,(make-symbol (phony--external-name rule))
    . ,(phony-dragonfly--serialize-rule-concrete rule)))

(defun phony-dragonfly--serialize-rules (dependency-data)
  `((dependency-linear-extension
     . ,(apply #'vector
                (seq-map
                 (lambda (rule) (phony--external-name rule))
                 (phony--dependency-data-linear-extension dependency-data))))
    (rules
     . ,(seq-map #'phony-dragonfly--serialize-rule
                 (phony--get-rules)))))

(defvar phony-dragonfly--rule-output-file nil)

(defun phony-dragonfly-export (dependency-data)
  (interactive (list (phony--analyze-grammar)))
  (with-temp-file phony-dragonfly--rule-output-file
    (json-insert (phony-dragonfly--serialize-rules dependency-data))
    (json-pretty-print-buffer)))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
