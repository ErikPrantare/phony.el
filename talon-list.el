;;; talon-list.el --- Declare talon lists            -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Erik Präntare

;; Author: Erik Präntare
;; Keywords: files
;; Version: 0.0.0
;; Package-Requires: ((emacs "27.1"))
;; Created: 13 Jul 2024

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; TODO
;; - Allow docstrings
;;   - Put docstring in list description talon-side
;; - Create visualization with hierarchy.el (for easier the bugging)
;; - Make output file defaultable
;; - Create subtree
;; - Put TODO in separate file
;; - Add documentation

(require 'cl-lib)

(defgroup talon-list nil
  "Functionality for defining talon lists in Emacs Lisp."
  :group 'files)

(defcustom talon-list-output-file "~/.talon/emacs-lists.json"
  "Output file for defined lists."
  :type 'file
  :group 'talon-list)

(defun talon-list--lookup (utterance list)
  "Return the value corresponding to UTTERANCE in LIST."
  (alist-get utterance list nil nil #'equal))

(defun talon-list--create-lookup-representation (utterance list-name)
  "Create lookup string for UTTERANCE in LIST-NAME.

When evaluating the returned value from emacsclient, this
performs the lookup."
  (format "(talon-list--lookup \"%s\" %s)"
          utterance
          list-name))

(defun talon-list--prepare-list-for-serialization (list-name)
  "Return list in LIST-NAME as an entry for `json-serialize'.

This represents one key-value pair, mapping the talon list name
to its list.  `json-serielize' will create a JSON objective
passed a list of such key-value pairs."
  (cons (get list-name 'talon-list--talon-name)
        (let ((mapping (symbol-value list-name)))
          (mapcar (lambda (entry)
                    (cons
                     ;; json-serialize expects symbols for keys.
                     (make-symbol (car entry))
                     ;; Each value is a string, encoding a form that
                     ;; will evaluate to the actual value.
                     (talon-list--create-lookup-representation
                      (car entry) list-name)))
                  mapping))))

;; TODO: Handle IO errors
(defun talon-list--send-lists (list-names)
  "Send lists coded in LIST-NAMES to `talon-list-output-file'.

Talon can read this file to register the lists."
  (with-temp-file talon-list-output-file
    (insert
     (json-serialize
      (mapcar #'talon-list--prepare-list-for-serialization list-names)))))

(defvar talon-list--list-names '()
  "All defined talon lists.")

(defun talon-list--define-list (list-name talon-name mapping)
  "Define list with LIST-NAME and TALON-NAME containing MAPPING.
Update `talon-list-output-file' to contain the definition."
  
  (eval `(defvar ,list-name))
  (setf (symbol-value list-name) mapping)
  (put list-name 'talon-list--talon-name talon-name)

  (add-to-list 'talon-list--list-names list-name)
  (talon-list--send-lists talon-list--list-names)

  list-name)

(defmacro define-talon-list (list talon-name mapping)
  "Define a LIST with TALON-NAME containing MAPPING.
Update `talon-list-output-file' to contain the definition.

MAPPING is an alist mapping utterances to values.  An utterance
is a string containing the spoken form for referencing the value.

MAPPING will be stored in the variable LIST."
  (declare (indent defun))
  `(talon-list--define-list ',list ',talon-name ,mapping))

(provide 'talon-list)
;;; talon-list.el ends here
