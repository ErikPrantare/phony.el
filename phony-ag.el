;;; phony-ag.el --- Attribute grammars for phony -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
;; Created: 25 Oct 2026

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

;; This file provides attribute grammar utilities for phony.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defvar phony-ag--parent-map (make-hash-table :test 'eq :weakness 'key)
  "Mapping from child to (PARENT . SLOT-DESCRIPTOR).")

(cl-defgeneric phony-ag--equation (_attribute _node _slot-descriptor)
  "Return value of inherited ATTRIBUTE for SLOT-DESCRIPTOR of NODE.

NODE is the parent defining ATTRIBUTE for the child matching
SLOT-DESCRIPTOR.

This function is specialized through `phony-ag--define-equation'.

By default, returns the symbol `phony-ag--undefined'."
  'phony-ag--undefined)

(cl-defmacro phony-ag--define-inherited (name (node) &optional docstring)
  "Define an inherited attribute NAME.

This macro defines a function NAME for accessing the attribute of NODE.
DOCSTRING supplies the documentation string of this accessor.

Inherited attributes are defined by an ancestor of a node through
`phony-ag--define-equation'.

For inherited attributes to work, call `phony-ag--initialize' on the
root node first."
  (declare (doc-string 3))
  `(eval-and-compile
     (let ((phony-ag--cache (make-hash-table :test 'eq :weakness 'key)))
       (defun ,name (,node)
         ,@(ensure-list docstring)
         ;; We do not use with-memoization here, because nil is a
         ;; legal result, and with-memoization would interpret that as
         ;; absence.
         (let ((cached-value (gethash ,node phony-ag--cache 'phony-ag--undefined)))
           (if (not (eq cached-value 'phony-ag--undefined))
               cached-value
             (let ((parent-entry (gethash ,node phony-ag--parent-map))
                   result)
               (if parent-entry
                   (setq result
                         (phony-ag--equation
                          ',name (car parent-entry) (cdr parent-entry)))
                 (error "No definer for inherited attribute `%s' found"
                        ',name))
               (when (eq result 'phony-ag--undefined)
                 (setq result (,name (car parent-entry))))
               (setf (gethash ,node phony-ag--cache) result)
               result)))))))

(cl-defmacro phony-ag--define-equation (attribute (node slot-descriptor) &rest body)
  "Define the implementation of inherited attribute ATTRIBUTE.

The attribute is the result of evaluating BODY.

This defines the attribute declared by `phony-ag--define-inherited' for
the descendants of NODE matching SLOT-DESCRIPTOR.  NODE and
SLOT-DESCRIPTOR have the same form and meaning as parameters to
`cl-defmethod'."
  `(cl-defmethod phony-ag--equation ((_attribute (eql ',attribute))
                                     ,node ,slot-descriptor)
     ,@body))

(cl-defmacro phony-ag--define-synthesized (name (node) docstring &rest body)
  "Define a synthesized attribute NAME.

This macro defines a generic function NAME for accessing the attribute
of NODE.  DOCSTRING supplies the documentation string of this accessor.
BODY optionally provides a default implementation for computing the
attribute.  Use `cl-defmethod' to define special cases."
  (declare (doc-string 3) (indent defun))
  `(eval-and-compile
     (let ((phony-ag--cache (make-hash-table :test 'eq :weakness 'key)))
       (cl-defgeneric ,name (,node) ,docstring)
       ;; HACK: If I put BODY in the generic, byte-compilation
       ;; complains about undefined
       ;; internal--with-suppressed-warnings.  I should investigate
       ;; this and/or report upstream...
       ,@(and body `((cl-defmethod ,name (,node) ,@body)))
       (cl-defmethod ,name :around (,node)
         ;; We do not use with-memoization here, because nil is a legal
         ;; result, and with-memoization would interpret that as absence.
         (let ((cached-value (gethash ,node phony-ag--cache 'phony-ag--undefined)))
           (if (not (eq cached-value 'phony-ag--undefined))
               cached-value
             (setf (gethash ,node phony-ag--cache) (cl-call-next-method))))))))

(cl-defgeneric phony-ag--children (node)
  "Return the children of NODE.

This returns an alist of entries (SLOT-DESCRIPTOR . CHILD).
SLOT-DESCRIPTOR is used in `phony-ag--define-equation' to match specific
children.

NODE must be a cl-struct.  By default, this function returns an entry
for each slot of NODE which is also a cl-struct, keyed by slot name."
  (unless (cl-struct-p node)
    (error "Can't automatically deduce children of non-struct %S" node))
  ;; Emacs 30.1+: Use cl-type-of
  (let* ((type (type-of node))
         (slots (mapcar #'car (cl-struct-slot-info type))))
    (seq-filter
     (lambda (entry) (cl-struct-p (cdr entry)))
     ;; First element is the type tag
     (cdr (seq-map-indexed
           (lambda (slot index) (cons slot (aref node index)))
           slots)))))

(defun phony-ag--initialize (node)
  "Initialize NODE.

This must be invoked on the root of the tree before other attributes are
evaluated on it or its descendants."
  (pcase-dolist (`(,slot-descriptor . ,child) (phony-ag--children node))
    (phony-ag--set-parent child node slot-descriptor)
    (phony-ag--initialize child)))

(defun phony-ag--set-parent (node parent slot-descriptor)
  "Set the parent of NODE to PARENT.
Set NODE as the child identified by SLOT-DESCRIPTOR."
  (puthash node (cons parent slot-descriptor) phony-ag--parent-map))

(provide 'phony-ag)
;;; phony-ag.el ends here
