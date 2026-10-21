;;; phony.el --- Define voice commands           -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
;; Keywords: files
;; Version: 2.2.0
;; Homepage: https://github.com/ErikPrantare/phony.el
;; Package-Requires: ((emacs "29.1") (simulacrum "2.0.0"))
;; Created: 13 Jul 2024

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phony is a framework for defining voice commands in Emacs.  It
;; bridges Emacs with speech recognition backends to allow you to
;; control Emacs by voice.
;;
;; Example:
;;
;;   (phony-defun say-hello "hello world"
;;     (message "Hello!"))
;;
;; After defining this function, saying "hello world" will display
;; "Hello!" in the message area.
;;
;; Phony provides different types of rules for mapping utterances to
;; values and side effects:
;;
;;   1. Procedure rules (`phony-defun') map utterances to code with
;;      optional arguments bound from other rules.
;;
;;   2. Dictionaries (`phony-define-dictionary') map utterances to
;;      values (e.g., numbers, symbols).
;;
;;   3. Open rules (`phony-define-open-rule') match any of multiple
;;      alternatives, allowing extensible grammars.
;;
;;   4. Interface rules (`phony-define-interface') allow substitution
;;      of rule definitions for user customization points.
;;
;; Rules can reference other rules to compose complex speech patterns.
;; The module system (`phony-module') allows you to organize and
;; enable/disable groups of rules.
;;
;; See README.org for setup instructions and detailed usage examples.

;;; Code:

(require 'phony-core)
(require 'phony-stdlib)
(require 'phony-stdlib-talon-community)

(autoload 'phony-experimental-mode "phony-experimental")
(autoload 'phony-talon-export "phony-talon")
(autoload 'phony-dragonfly-export "phony-dragonfly")

(provide 'phony)
;;; phony.el ends here
