;;; phony-talon-community-bindings.el --- Basic bindings for rules in talon community  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

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

(require 'phony)

(defmacro phony-talon-community-bindings--make (name external-name)
  "Define a non-interactive phony rule wrapping EXTERNAL-NAME under NAME."
  (let ((docstring (format "Phony binding for Talon community capture `%s'."
                           (mapconcat #'symbol-name external-name "."))))
    `(phony-defun ,name ((x (external-rule ,@external-name)))
       ,docstring
       :interactive nil
       x)))

(phony-talon-community-bindings--make number (user number))
(phony-talon-community-bindings--make word (user word))
(phony-talon-community-bindings--make phrase (user phrase))
(phony-talon-community-bindings--make digit (user digit))

(phony-defun letter ((char (external-rule user letter)))
  "Phony binding for Talon community capture `user.letter'."
  :interactive nil
  (seq-first char))

(phony-defun any-alphanumeric-key ((char (external-rule user any_alphanumeric_key)))
  "Phony binding for Talon community capture `user.any_alphanumeric_key'."
  :interactive nil
  (seq-first char))

(phony-defun symbol-key ((char (external-rule user symbol_key)))
  "Phony binding for Talon community capture `user.symbol_key'."
  :interactive nil
  (seq-first char))

(provide 'phony-talon-community-bindings)
;;; phony-talon-community-bindings.el ends here
