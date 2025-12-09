;;; phony-talon-community-bindings.el --- Basic bindings for rules in talon community  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience

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

(defmacro phony-talon-community-bindings--make (name external-name)
  `(defun ,name (x)
     (declare (phony-rule
               :export nil
               (x (external-rule ,@external-name))))
     x))

(phony-talon-community-bindings--make rule/number (user number))
(phony-talon-community-bindings--make rule/word (user word))
(phony-talon-community-bindings--make rule/phrase (user phrase))
(phony-talon-community-bindings--make rule/digit (user digit))
(phony-talon-community-bindings--make rule/number (user number))

(phony-defun letter ((char (external-rule user letter)))
  :export nil
  (seq-first char))

(phony-defun any-alphanumeric-key ((char (external-rule user any_alphanumeric_key)))
  :export nil
  (seq-first char))

(phony-defun symbol-key ((char (external-rule user symbol_key)))
  :export nil
  (seq-first char))

(provide 'phony-talon-community-bindings)
;;; phony-talon-community-bindings.el ends here
