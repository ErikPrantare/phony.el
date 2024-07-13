;;; talon-list.el --- Declare talon lists from emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Erik Präntare

;; Author: Erik Präntare
;; Keywords: convenience, abbrev
;; Version: 0.0.0

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
;; - Create visualization with hierarchy.el
;; - Make output file defaultable
;; - Create subtree
;; - Put TODO in separate file
;; - Add documentation

(require 'cl-lib)

;; TODO: Correctly handle errors (like?)
(defun my/talon-send-lists (file lists)
  "Sends LISTS to FILE.  Talon may read this to specify lists."
  (with-temp-file file
    (thread-last
      lists
      (seq-map (lambda (list)
                 (cons (my/talon-list-talon-name list)
                       (seq-map (lambda (entry)
                                  (cons
                                   (intern (car entry))
                                   (my/talon-create-lookup-representation
                                    (my/talon-list-emacs-name list)
                                    (car entry))))
                                (my/talon-list-list list)))))
      json-serialize
      insert)))

(defun my/talon-create-lookup-representation (emacs-name utterance)
  "Create lookup string in list EMACS-NAME for key UTTERANCE.

When evaluated through emacsclient, this corresponds to an
expression looking up the value in EMACS-NAME."
  (format "(my/talon-list-lookup '%s \"%s\")"
          emacs-name
          utterance))

(defvar my/talon-lists '())

(defun my/talon-list-lookup (emacs-name utterance)
  (thread-last
    my/talon-lists
    (seq-find (lambda (list) (equal (my/talon-list-emacs-name list) emacs-name)))
    my/talon-list-list
    (assoc utterance)
    cdr))

(cl-defstruct my/talon-list
  list emacs-name talon-name output-file)

(defun my/talon-set-list (emacs-name talon-name output-file list)
  (let ((old-list (seq-find
                   (lambda (list) (eq emacs-name (my/talon-list-emacs-name list)))
                   my/talon-lists)))
    (if (not old-list)
        (setq my/talon-lists (cons (make-my/talon-list
                                   :emacs-name emacs-name
                                   :talon-name talon-name
                                   :output-file output-file
                                   :list list)
                                   my/talon-lists))
      (setf (my/talon-list-list old-list) list)
      (setf (my/talon-list-talon-name old-list) talon-name)
      (setf (my/talon-list-output-file old-list) output-file)))
  (thread-last
    my/talon-lists
    (seq-filter (lambda (list) (equal output-file
                                     (my/talon-list-output-file list))))
    (my/talon-send-lists output-file)))

;; TODO only specify output file optionally.
(defmacro my/define-talon-list (emacs-name talon-name output-file talon-list)
  (declare (indent defun))
  (let ((talon-list-variable (make-symbol "talon-list-variable")))
    `(let ((,talon-list-variable ,talon-list))
       (my/talon-set-list (quote ,emacs-name)
                          (quote ,talon-name)
                          ,output-file
                          ,talon-list-variable)

       (defvar ,emacs-name)
       (setq ,emacs-name (seq-map #'cdr ,talon-list-variable)))))

(provide 'talon-list)
;;; talon-list.el ends here
