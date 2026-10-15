;;; phony-stdlib.el --- Standard rules for phony.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Erik Präntare

;; Author: Erik Präntare <erik@prantare.xyz>
;; Created: 11 Sep 2026

;; phony-stdlib.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Affero General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; phony-stdlib.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU Affero General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; phony-stdlib.el provides rules that are useful as part of other
;; grammars.

;;; Code:

(require 'phony-core)

(phony-module phony-stdlib)

(phony-define-interface word
  "A word."
  :default phony-stdlib-word)

(phony-define-interface phrase
  "A sequence of words, returned as a string.

This rule intends to capture parts of sentences."
  :default phony-stdlib-phrase)

(phony-define-interface digit
  "A number between 0 and 9."
  :default phony-stdlib-digit)

(phony-define-interface number
  "A number."
  :default phony-stdlib-number)

(phony-define-interface letter
  "A letter."
  :default phony-stdlib-letter)

(phony-define-interface non-alphanumeric-character
  "A graphical character that is not alphanumeric."
  :default phony-stdlib-non-alphanumeric-character)

(phony-defun phony-stdlib-word ((word phony--builtin-word))
  "A word.

Returns WORD directly."
  :interactive nil
  :provides word
  word)

(phony-defun phony-stdlib-phrase ((phrase phony--builtin-phrase))
  "A phrase.

Returns PHRASE directly."
  :interactive nil
  :provides phrase
  phrase)

(phony-define-dictionary phony-stdlib-digit
  "A number between 0 and 9."
  :provides digit
  '(("zero" . 0)
    ("one" . 1)
    ("two" . 2)
    ("three" . 3)
    ("four" . 4)
    ("five" . 5)
    ("six" . 6)
    ("seven" . 7)
    ("eight" . 8)
    ("nine" . 9)))

(phony-define-dictionary phony-stdlib--teens
  "A number between 10 and 19."
  (seq-map-indexed
   (lambda (utterance index)
     (cons utterance (+ 10 index)))
   (string-split "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen")))

(phony-define-dictionary phony-stdlib--tens
  "A number between 20 and 90 divisible by 10."
  (seq-map-indexed
   (lambda (utterance index)
     (cons utterance (* 10 (+ 2 index))))
   (string-split "twenty thirty forty fifty sixty seventy eighty ninety")))

(phony-define-dictionary phony-stdlib--numbers-scale
  "A number 10^(3*n), or 100."
  (seq-map-indexed
   (lambda (utterance index)
     (cons utterance
           (if (= index 0)
               100
             (expt 1000 index))))
   (string-split "hundred thousand million billion trillion quadrillion quintillion sextillion septillion octillion nonillion decillion")))

(phony-define-dictionary phony-stdlib--number-component
  "A number that partakes in the utterance of a number."
  ;; TODO: Make this an open rule whenever phony can optimize it into
  ;; a dict.
  (append (phony-dictionary-alist 'phony-stdlib-digit)
          (phony-dictionary-alist 'phony-stdlib--teens)
          (phony-dictionary-alist 'phony-stdlib--tens)
          (phony-dictionary-alist 'phony-stdlib--numbers-scale)))

(defun phony-stdlib--collapse-tens (parts)
  "Compose tens followed by digits in PARTS.

For example, if PARTS is

  (1 20 7 100 30 2)

this function returns
  (1 27 100 32)."
  (let ((result nil))
    (while parts
      (let ((next (pop parts)))
        (cond
         ((and (<= 20 next 90)
               (car parts)
               (<= 1 (car parts) 9))
          (push (+ next (pop parts)) result))
         (t (push next result)))))
    (nreverse result)))

(defun phony-stdlib--concatenate-numbers (numbers)
  "Return the concatenation of NUMBERS.

The concatenation is the number whose string representation is the
concatenation of the string representations of NUMBERS."
  (string-to-number
   (apply #'concat
          (mapcar #'number-to-string numbers))))

(defun phony-stdlib--collapse-scale (parts scale)
  "Compose SCALE with its prefix and postfix in PARTS.

A prefix is a sequence of numbers preceding SCALE.  Composing a prefix
of SCALE means concatenating the digits of the number sequence and
multiplying SCALE by it.  A postfix works the same way, but occurs after
SCALE and is added instead.

For example, if SCALE is 1000 and PARTS is

  (823 1000 5 5)

the function returns

  (823055)."
  (let (result)
    (while parts
      (let* ((prefix (seq-take-while
                      (lambda (x) (< x scale))
                      parts))
             (rest (seq-drop parts (seq-length prefix))))
        (cond
         ((eql scale (car rest))
          (let (postfix)
            (setq parts rest)
            (pop parts)
            (setq postfix
                  (seq-take-while
                   (lambda (x) (< x scale))
                   parts))
            (setq parts (seq-drop parts (seq-length postfix)))
            (setq prefix (or prefix '(1)))
            (setq postfix (or postfix '(0)))
            (push (+ (* (phony-stdlib--concatenate-numbers prefix)
                        scale)
                     (phony-stdlib--concatenate-numbers postfix))
                  result)))
         (t (push (pop parts) result)))))
    (nreverse result)))

(defun phony-stdlib--parse-number (parts)
  "Parse PARTS, a number sequence, into a number.

PARTS corresponds to how the number would be uttered.  E.g.,

  (2 1000 5 100 20 3)

parses into 2523."
  (setq parts (phony-stdlib--collapse-tens parts))
  (let ((scales (seq-map #'cdr (phony-dictionary-alist
                                'phony-stdlib--numbers-scale))))
    (seq-doseq (scale scales)
      (setq parts (phony-stdlib--collapse-scale parts scale))))
  (string-to-number
   (string-join (seq-map #'number-to-string parts))))

(phony-defun phony-stdlib--number-string
    ((+ (? "and") (components phony-stdlib--number-component)))
  "A string containing a number, parsed from COMPONENTS.

If COMPONENTS contains leading zeroes, the number string does as well."
  :interactive nil
  (let* ((parts components)
         (initial-zeroes (seq-take-while (apply-partially #'= 0) parts))
         (initial-zero-string (seq-into (seq-map (lambda (_) ?0) initial-zeroes) 'string)))
    (if (= (seq-length initial-zeroes) (seq-length parts))
        initial-zero-string
      (concat initial-zero-string
              (number-to-string (phony-stdlib--parse-number parts))))))

(phony-defun phony-stdlib-number ((number-string phony-stdlib--number-string))
  "A number.

NUMBER-STRING is parsed by `string-to-number'."
  :interactive nil
  :provides number
  (string-to-number number-string))

(phony-define-dictionary phony-stdlib-letter
  "A letter."
  :provides letter
  '(("air" . ?a)
    ("bat" . ?b)
    ("cap" . ?c)
    ("drum" . ?d)
    ("each" . ?e)
    ("fine" . ?f)
    ("gust" . ?g)
    ("harp" . ?h)
    ("sit" . ?i)
    ("jury" . ?j)
    ("crunch" . ?k)
    ("look" . ?l)
    ("made" . ?m)
    ("near" . ?n)
    ("odd" . ?o)
    ("pit" . ?p)
    ("quench" . ?q)
    ("red" . ?r)
    ("sun" . ?s)
    ("trap" . ?t)
    ("urge" . ?u)
    ("vest" . ?v)
    ("whale" . ?w)
    ("plex" . ?x)
    ("yank" . ?y)
    ("zip" . ?z)))

(phony-define-dictionary phony-stdlib-non-alphanumeric-character
  "A graphical character that is not alphanumeric."
  :provides non-alphanumeric-character
  '(("grave" . ?`)
    ("dot" . ?.)
    ("semi" . ?\;)
    ("colon" . ?:)
    ("comma" . ?,)
    ("at sign" . ?@)
    ("question" . ??)
    ("bang" . ?!)
    ("star" . ?*)
    ("hash" . ?#)
    ("percent" . ?%)
    ("degree" . ?°)
    ("amper" . ?&)
    ("dash" . ?-)
    ("equals" . ?=)
    ("plus" . ?+)
    ("tilde" . ?~)
    ("underscore" . ?_)
    ("square" . ?\[)
    ("right square" . ?\])
    ("paren" . ?\()
    ("right paren" . ?\))
    ("brace" . ?{)
    ("right brace" . ?})
    ("angle" . ?<)
    ("right angle" . ?>)
    ("slash" . ?/)
    ("backslash" . ?\\)
    ("caret" . ?^)
    ("pipe" . ?|)
    ("quote" . ?')
    ("double quote" . ?\")
    ("dollar" . ?$)
    ("pound" . ?£)
    ("euro" . ?€)))

(phony-defun phony-stdlib--digit-character (digit)
  "A character encoding a digit between 0 and 9."
  :interactive nil
  (+ ?0 digit))

(phony-define-open-rule phony-stdlib--graphical-character
  "A graphical character."
  ;; TODO: Perhaps a :closed parameter would be nice?  To avoid the
  ;; indirection for graphical-character.
  :alternatives
  (letter phony-stdlib--digit-character non-alphanumeric-character))

(phony-defun graphical-character (phony-stdlib--graphical-character)
  "A graphical character."
  ;; We add indirection to signal that the open is inextensible.
  :interactive nil
  phony-stdlib--graphical-character)

(phony-defun any-alphanumeric-key (phony-stdlib--graphical-character)
  "A graphical character.

This rule is obsolete.  Use `graphical-character' instead."
  :interactive nil
  ;; TODO: Mark :obsolete when added to phony.
  phony-stdlib--graphical-character)

(phony-defun symbol-key (non-alphanumeric-character)
  "A graphical character that is not alphanumeric.

This rule is obsolete.  Use `non-alphanumeric-character' instead."
  :interactive nil
  ;; TODO: Mark :obsolete when added to phony.
  non-alphanumeric-character)

(provide 'phony-stdlib)
;;; phony-stdlib.el ends here
