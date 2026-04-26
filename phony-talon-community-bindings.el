;;; phony-talon-community-bindings.el --- Basic bindings for rules in talon community  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience

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

(defmacro phony-talon-community-bindings--make (name external-name)
  `(phony-defun ,name ((x (external-rule ,@external-name)))
     :export nil
     x))

(phony-talon-community-bindings--make number (user number))
(phony-talon-community-bindings--make word (user word))
(phony-talon-community-bindings--make phrase (user phrase))
(phony-talon-community-bindings--make digit (user digit))

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
