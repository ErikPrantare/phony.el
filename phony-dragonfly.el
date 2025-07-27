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

(cl-defgeneric phony-dragonfly--serialize-rule-concrete (rule))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--procedure-rule))
  (symbol-name (phony--procedure-rule-function rule)))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--open-rule))
  (phony--open-rule-talon-name rule))

(defun phony-dragonfly--serialize-rule (rule)
  (cons (make-symbol (phony--rule-talon-name rule))
        (phony-dragonfly--serialize-rule-concrete rule)))

(defun phony-dragonfly-export ()
  (let* ((rules (hash-table-values phony--rules)))
    (with-temp-file "~/temp/rules.json"
      (json-insert
       (seq-map #'phony-dragonfly--serialize-rule rules)))))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
