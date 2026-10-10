;;; phony-stdlib-talon-community.el --- Basic bindings for rules in talon community  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
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

(require 'phony-core)
(require 'phony-stdlib)

(phony-module phony-stdlib-talon-community)

(phony-defun phony-stdlib-talon-community--word ((word (external-rule user word)))
  "Return WORD, a string.

This rule uses the Talon community capture `user.word'."
  :interactive nil
  :provides word
  word)

(phony-defun phony-stdlib-talon-community--phrase ((phrase (external-rule user phrase)))
  "Return PHRASE, a string.

This rule uses the Talon community capture `user.phrase'."
  :interactive nil
  :provides phrase
  phrase)

(phony-defun phony-stdlib-talon-community--number ((number (external-rule user number)))
  "Return NUMBER.

This rule uses the Talon community capture `user.number'."
  :interactive nil
  :provides number
  number)

(phony-defun phony-stdlib-talon-community--digit ((char (external-rule user number_key)))
  "Return CHAR as a number between 0 and 9.

This rule uses the Talon community capture `user.number_key'."
  :interactive nil
  :provides digit
  (- (string-to-char char) ?0))

(phony-defun phony-stdlib-talon-community--letter ((char (external-rule user letter)))
  "Return letter CHAR.

This rule uses the Talon community capture `user.letter'."
  :interactive nil
  :provides letter
  (string-to-char char))

(phony-defun phony-stdlib-talon-community--non-alphanumeric-character
    ((char (external-rule user symbol_key)))
  "Return non-alphanumeric graphical character CHAR.

This rule uses the Talon community capture `user.symbol_key'."
  :interactive nil
  :provides non-alphanumeric-character
  (string-to-char char))

(provide 'phony-stdlib-talon-community)
;;; phony-stdlib-talon-community.el ends here
