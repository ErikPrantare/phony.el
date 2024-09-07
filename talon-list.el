;;; talon-list.el --- Declare talon lists from emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Erik Präntare

;; Author: Erik Präntare
;; Keywords: files
;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1"))
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
;; - Create visualization with hierarchy.el
;; - Make output file defaultable
;; - Create subtree
;; - Put TODO in separate file
;; - Add documentation

(require 'cl-lib)
(require 'subr-x)

;; TODO: Correctly handle errors (like?)
(defun talon-list--send-lists (file lists)
  "Sends LISTS to FILE.  Talon may read this to specify lists."
  (with-temp-file file
    (thread-last
      lists
      (seq-map (lambda (list)
                 (cons (talon-list--list-talon-name list)
                       (seq-map (lambda (entry)
                                  (cons
                                   (intern (car entry))
                                   (talon-list--create-lookup-representation
                                    (talon-list--list-emacs-name list)
                                    (car entry))))
                                (talon-list--list-mapping list)))))
      json-serialize
      insert)))

(defun talon-list--create-lookup-representation (emacs-name utterance)
  "Create lookup string in list EMACS-NAME for key UTTERANCE.

When evaluated through emacsclient, this corresponds to an
expression looking up the value in EMACS-NAME."
  (format "(my/talon-list-lookup '%s \"%s\")"
          emacs-name
          utterance))

(defvar talon-list--lists '())

(defun talon-list--lookup (emacs-name utterance)
  (thread-last
    talon-list--lists
    (seq-find (lambda (list) (equal (talon-list--list-emacs-name list) emacs-name)))
    talon-list--list-mapping
    (assoc utterance)
    cdr))

(cl-defstruct talon-list--list
  mapping emacs-name talon-name output-file (docstring nil))



(defun talon-list--set-list (emacs-name talon-name output-file mapping)
  (let ((old-list (seq-find
                   (lambda (list) (eq emacs-name (talon-list--list-emacs-name list)))
                   talon-list--lists)))
    (if (not old-list)
        (setq talon-list--lists (cons (make-talon-list--list
                                    :emacs-name emacs-name
                                    :talon-name talon-name
                                    :output-file output-file
                                    :mapping mapping)
                                   talon-list--lists))
      (setf (my/talon-list-mapping old-list) mapping)
      (setf (my/talon-list-talon-name old-list) talon-name)
      (setf (my/talon-list-output-file old-list) output-file)))
  (thread-last
    talon-list--lists
    (seq-filter (lambda (list) (equal output-file
                                      (talon-list--list-output-file list))))
    (talon-list--send-lists output-file)))

;; TODO only specify output file optionally.
(defmacro define-talon-list (emacs-name talon-name output-file talon-list)
  (declare (indent defun))
  (let ((talon-list-variable (make-symbol "talon-list-variable")))
    `(let ((,talon-list-variable ,talon-list))
       (my/talon-set-list (quote ,emacs-name)
                          (quote ,talon-name)
                          ,output-file
                          ,talon-list-variable)

       (defvar ,emacs-name)
       (setq ,emacs-name ,talon-list-variable))))

(provide 'talon-list)
;;; talon-list.el ends here
