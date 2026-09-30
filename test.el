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
(require 'phony-parser)
(require 'phony-talon)

(defmacro phony-test (&rest body)
  "Evaluate BODY in a fresh phony test environment.

In the environment, `phony-mode' is disabled, no rules are bound,
exporters do nothing, and export request debounce is turned off."
  (declare (indent 0))
  `(let ((phony--default-grammar (phony--make-grammar))
         (phony--exported-grammar (phony--make-grammar))
         (phony-export-function #'ignore)
         (phony--dictionary-export-function #'ignore)
         (phony--debounce-export-requests nil)
         (phony-mode-was-enabled phony-mode)
         (phony--last-analysis nil))
     (unwind-protect
         (progn
           (phony-mode 0)
           ,@body)
       ;; The export happening in (phony-mode 1) is fine, as it is
       ;; done still in our test environment.
       (if phony-mode-was-enabled
           (phony-mode 1)
         (phony-mode 0)))))

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
    (let* ((dictionary-export-count 0)
           (rule-export-count 0)
           (phony--dictionary-export-function
            (lambda () (cl-incf dictionary-export-count)))
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (phony-mode 1)
      (phony-define-dictionary test-dict
        '(("alpha" . 1)))
      (should (= dictionary-export-count 1))
      (should (= rule-export-count 0)))))

(ert-deftest phony-open-rule-definition-triggers-export ()
  "Defining an open rule triggers rule export."
  (phony-test
    (let* ((rule-export-count 0)
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (phony-mode 1)
      (cl-letf (((symbol-function 'server-running-p) #'always))
        (phony-define-open-rule test-open-rule))
      (should (= rule-export-count 1)))))

(ert-deftest phony-procedure-rule-definition-triggers-export ()
  "Defining a procedure rule triggers rule export."
  (phony-test
    (let* ((rule-export-count 0)
           (phony-export-function
            (lambda (_analysis-data) (cl-incf rule-export-count))))
      (phony-mode 1)
      (cl-letf (((symbol-function 'server-running-p) #'always))
        (phony-defun test-procedure-rule "alpha"
          t))
      (should (= rule-export-count 1)))))

(ert-deftest phony-dictionary-modification-triggers-export ()
  "Modifying a dictionary only triggers dictionary export."
  (phony-test
    (phony-mode 1)
    (phony-define-dictionary test-dict
      '(("alpha" . 1)))
    (let* ((dictionary-export-count 0)
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

(defun phony-test--evaluate (sentence)
  "Return the result of parsing and evaluating SENTENCE.

SENTENCE must parse unambiguously into a single command.

If there is no parse, an error is thrown."
  (let ((parses (phony-parser--parse-utterance sentence)))
    (unless parses
      (error "No parse available for '%s'" sentence))
    (phony--evaluate-ast (caar parses))))

(ert-deftest phony-open-rule-contribution ()
  "Rule contributions work."
  (phony-test
    (phony-define-open-rule number)
    (phony-defun three "three"
      :contributes-to number
      3)
    (phony-defun five "five"
      :contributes-to number
      5)
    (phony-defun add ("add" (first number) "and" (second number))
      (+ first second))
    (phony--export-all)
    (should (= (phony-test--evaluate "add five and three") 8))
    (should (= (phony-test--evaluate "add five and five") 10))
    (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-interface-uses-provider ()
  "Providers are used in place of interfaces."
  (phony-test
    (phony-define-interface number)
    (phony-defun my-number "three"
      :provides number
      3)
    (phony-defun double ("double" number)
      (* 2 number))
    (phony--export-all)
    (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-interface-nestled ()
  "Nestled interfaces resolve."
  (phony-test
   (phony-define-interface number)
   (phony-define-interface real
     :provides number)
   (phony-define-dictionary natural
     :provides real
     '(("elite" . 1337)))
   (phony-defun double ("double" number)
     (* 2 number))
   (phony--export-all)
   (should (= (phony-test--evaluate "double elite") 2674))))

(ert-deftest phony-interface-cycle-detection ()
  "Cyclic interfaces error."
  (phony-test
   (phony-define-interface number
     :provides number)
   (phony-defun double ("double" number)
     (* 2 number))
   (should-error (phony--export-all) :type 'phony--interface-cycle)))

(ert-deftest phony-interface-uses-selected-provider ()
  "Only the selected provider is used for interface."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module three-module)
   (phony-define-module five-module)

   (phony-defun three "three"
     :module three-module
     :provides number
     3)
   (phony-defun five "five"
     :module five-module
     :provides number
     5)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-provide-from number-module three-module)
   (phony--export-all)
   (should (= (phony-test--evaluate "double three") 6))
   (should-error (phony-test--evaluate "double five"))

   (phony-provide-from number-module five-module)
   (phony--export-all)
   (should-error (phony-test--evaluate "double three"))
   (should (= (phony-test--evaluate "double five") 10))))

(ert-deftest phony-interface-selects-user-provider ()
  "No-module providers are preferred over module providers."
  (phony-test
   (phony-define-module number-module)
   (phony-define-module some-module)
   (phony-define-interface number
     :module number-module)
   (phony-defun my-number "three"
     :provides number
     3)
   (phony-defun module-number "five"
     :provides number
     :module some-module)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-provide-from number-module some-module)
   (phony--export-all)
   (should (= (phony-test--evaluate "double three") 6))))

(ert-deftest phony-interface-error-on-multiple-providers ()
  "Interfaces can't resolve on multiple providers."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module my-numbers)

   (phony-defun three "three"
     :module my-numbers
     :provides number
     3)
   (phony-defun five "five"
     :module my-numbers
     :provides number
     5)
   (phony-defun double ("double" number)
     (* 2 number))

   (phony-provide-from number-module my-numbers)
   (should-error (phony--export-all))))

(ert-deftest phony-interface-error-on-no-selected-provider ()
  "Reachable interfaces must have a selected provider."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module three-module)
   (phony-define-module five-module)

   (phony-defun three "three"
     :module three-module
     :provides number
     3)
   (phony-defun five "five"
     :module five-module
     :provides number
     5)
   (phony-defun double ("double" number)
     (* 2 number))

   (should-error (phony--export-all))))

(ert-deftest phony-interface-elide-unused ()
  "Interfaces with more or less than one provider is ok if unreachable."
  (phony-test
   (phony-define-module number-module)
   (phony-define-interface number
     :module number-module)
   (phony-define-module my-numbers)

   (phony-defun three "three"
     :module my-numbers
     :provides number
     3)
   (phony-defun five "five"
     :module my-numbers
     :provides number
     5)
   (phony-defun double ("double" "four")
     (* 2 4))

   (phony--export-all)
   (should (= (phony-test--evaluate "double four") 8))

   (phony-provide-from number-module my-numbers)
   (phony--export-all)
   (should (= (phony-test--evaluate "double four") 8))))

(ert-deftest phony-interface-multiple-consumptions ()
  "Interfaces may be consumed multiple times."
  (phony-test
   (phony-define-interface number)

   (phony-defun three "three"
     :provides number
     3)
   (phony-defun sum ("add" (x number) "and" (y number))
     (+ x y))

   (phony--export-all)
   (should (= (phony-test--evaluate "add three and three") 6))))

(ert-deftest phony-builtin-word ()
  "Word builtins parse."
  (phony-test
   (phony-defun some-word (phony-builtin-word)
     phony-builtin-word)
   (phony--export-all)
   (should (equal (phony-test--evaluate "cwambabala")
                  "cwambabala"))))

(ert-deftest phony-builtin-phrase ()
  "Phrase builtins parse."
  (phony-test
   (phony-defun some-phrase (phony-builtin-phrase)
     phony-builtin-phrase)
   (phony--export-all)
   (should (equal (phony-test--evaluate "abra ha da bruh")
                  "abra ha da bruh"))))

(ert-deftest phony-disabled-rules-no-contribute-externally ()
  "Disabled rules do not contribute to open rules outside module."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :module x
     :contributes-to z
     5)
   (phony-define-open-rule z)
   (phony-defun y ("y" z)
     (+ z 3))
   (phony-enable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))
   (phony-disable-module 'x)
   (phony--export-all)
   (should-error (phony-test--evaluate "y x"))))

(ert-deftest phony-disabled-rules-contribute-internally ()
  "Disabled rules do contribute to open rules inside module."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :module x
     :contributes-to z
     5)
   (phony-define-open-rule z
     :module x)
   (phony-defun y ("y" z)
     (+ z 3))
   (phony-enable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))
   (phony-disable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))))

(ert-deftest phony-disabled-rules-may-be-used-explicitly ()
  "Disabled rules can still be used explicitly."
  (phony-test
   (phony-define-module x)
   (phony-defun x "x"
     :module x
     5)
   (phony-defun y ("y" x)
     (+ x 3))
   (phony-disable-module 'x)
   (phony--export-all)
   (should (equal (phony-test--evaluate "y x") 8))))

(ert-deftest phony-talon-export ()
  "The talon exporter exports with correct syntax."
  (unless (executable-find "python")
    (let ((message "Python is not installed in the environment"))
      (if noninteractive
          (error "%s" message)
        (ert-skip message))))
  (phony-test
   (let* ((directory (make-temp-file "phony-talon-test" t))
          (phony-output-directory directory))
     (unwind-protect
         (progn
           (phony-define-module some-module)
           (phony-enable-module 'some-module)
           (phony-define-open-rule number)
           (phony-defun three "three"
             :contributes-to number
             :module some-module
             3)
           (phony-defun five "five"
             :contributes-to number
             3)
           (phony-define-dictionary more-numbers
             :contributes-to number
             '(("pi" . 3.14) ("elite" . 1337)))
           (phony-defun double ("double" number)
             (* 2 number))
           (phony--export-all)
           (phony-talon--generate-configuration
            (phony--grammar-analysis phony--exported-grammar))
           (with-temp-buffer
             (unless (zerop (call-process "python" nil t nil "-m" "py_compile"
                                          (phony-talon--output-path "rules.py")
                                          (phony-talon--output-path "evaluate_elisp.py")
                                          (phony-talon--output-path "read_dictionaries.py")))
               (error "Syntax error in python: %s" (buffer-string)))))
       (delete-directory directory t)))))

;;; test.el ends here
