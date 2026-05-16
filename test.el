;;; test.el --- Tests for phony                      -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; This file is part of phony.

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

;;; Code:

(require 'ert)
(require 'phony)

(defmacro phony-test (&rest body)
  "Evaluate BODY in a fresh phony test environment.

In the environment, no rules are bound, exporters are inert, export
requests are suppressed, and export request debounce is turned off."
  (declare (indent 0))
  `(let ((phony--rules (make-hash-table))
         (phony--deny-export-requests-p t)
         (phony-export-function #'ignore)
         (phony--dictionary-export-function #'ignore)
         (phony--debounce-export-requests nil))
     ,@body))

(ert-deftest phony-dictionary-get-setf-roundtrip ()
  "Setting a dictionary entry via setf is readable via the getter."
  (phony-test
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let ((expected 42))
      (setf (phony-dictionary-get "alpha" 'test-dict) expected)
      (should (equal expected (phony-dictionary-get "alpha" 'test-dict))))))

(ert-deftest phony-dictionary-alist-setf-roundtrip ()
  "Setting a dictionary alist via setf is readable via the getter."
  (phony-test
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let ((expected '(("beta" . 2) ("gamma" . 3))))
      (setf (phony-dictionary-alist 'test-dict) expected)
      (should (equal expected (phony-dictionary-alist 'test-dict))))))

(ert-deftest phony-dictionary-definition-triggers-export ()
  "Defining a dictionary only triggers dictionary export."
  (phony-test
    (let* ((phony--deny-export-requests-p nil)
           (dictionary-export-count 0)
           (rule-export-count 0)
           (phony--dictionary-export-function
            (lambda () (cl-incf dictionary-export-count)))
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (phony-define-dictionary test-dict
        '(("alpha" . 1)))
      (should (= dictionary-export-count 1))
      (should (= rule-export-count 0)))))

(ert-deftest phony-open-rule-definition-triggers-export ()
  "Defining an open rule triggers rule export."
  (phony-test
    (let* ((phony--deny-export-requests-p nil)
           (rule-export-count 0)
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (cl-letf (((symbol-function 'server-running-p) #'always))
        (phony-define-open-rule test-open-rule))
      (should (= rule-export-count 1)))))

(ert-deftest phony-procedure-rule-definition-triggers-export ()
  "Defining a procedure rule triggers rule export."
  (phony-test
    (let* ((phony--deny-export-requests-p nil)
           (rule-export-count 0)
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (cl-letf (((symbol-function 'server-running-p) #'always))
        (phony-defun test-procedure-rule "alpha"
          t))
      (should (= rule-export-count 1)))))

(ert-deftest phony-dictionary-modification-triggers-export ()
  "Modifying a dictionary only triggers dictionary export."
  (phony-test
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let* ((phony--deny-export-requests-p nil)
           (dictionary-export-count 0)
           (rule-export-count 0)
           (phony--dictionary-export-function
            (lambda () (cl-incf dictionary-export-count)))
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (phony-dictionary-put "beta" 'test-dict 2)
      (should (= dictionary-export-count 1))
      (should (= rule-export-count 0))
      (phony-dictionary-remove "beta" 'test-dict)
      (should (= dictionary-export-count 2))
      (should (= rule-export-count 0))
      (phony-set-dictionary-alist 'test-dict '(("gamma" . 3)))
      (should (= dictionary-export-count 3))
      (should (= rule-export-count 0)))))

;;; test.el ends here
