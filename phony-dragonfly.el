;;; phony-dragonfly.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience
;; Created: 26 Jul 2025

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(cl-defgeneric phony-dragonfly--serialize-pattern (pattern)
  `((type . "undefined")))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-literal))
  `((type . "literal")
    (utterance . ,(phony--element-literal-string literal))))

(cl-defmethod phony-dragonfly--serialize-pattern ((variable phony--element-argument))
  `((type . "argument")
    (name . ,(symbol-name
              (phony--element-argument-name variable)))
    (rule . ,(phony-dragonfly--serialize-pattern
              (phony--element-argument-form variable)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((dictionary phony--element-rule))
  `((type . "rule")
    (name . ,(phony--external-name
              (phony--element-rule-name dictionary)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-one-or-more))
  `((type . "one-or-more")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-zero-or-more))
  `((type . "zero-or-more")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-optional))
  `((type . "optional")
    (element . ,(phony-dragonfly--serialize-pattern
                 (phony--element-compound-forms literal)))))

(cl-defmethod phony-dragonfly--serialize-pattern ((literal phony--element-external-rule))
  `((type . "impossible")))

(cl-defmethod phony-dragonfly--serialize-pattern ((pattern list))
  `((type . "sequence")
    (elements . ,(seq-into (seq-map #'phony-dragonfly--serialize-pattern pattern)
                           'vector))))

(cl-defgeneric phony-dragonfly--serialize-rule-concrete (rule))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--procedure-rule))
  `((type . "procedure-definition")
    (name . ,(phony--external-name rule))
    (function . ,(symbol-name (phony--procedure-rule-function rule)))
    (pattern . ,(phony-dragonfly--serialize-pattern
                 (phony--procedure-rule-elements rule)))
    (argument-list . ,(seq-into
                       (seq-map #'symbol-name
                                (phony--procedure-rule-arglist rule))
                       'vector))
    (export . ,(if (phony--procedure-rule-export rule) t :false))))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--open-rule))
  `((type . "open")
    (name . ,(phony--external-name rule))
    (alternatives . ,(seq-into (seq-map #'phony--external-name
                                        (phony--open-rule-alternatives rule))
                               'vector))))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--dictionary))
  `((type . "dictionary")
    (name . ,(phony--external-name rule))))

(defun phony-dragonfly--serialize-rule (rule)
  `(,(make-symbol (phony--external-name rule))
    . ,(phony-dragonfly--serialize-rule-concrete rule)))

(defun phony-dragonfly--serialize-rules (dependency-data)
  `((dependency-linear-extension
     . ,(apply #'vector
                (seq-map
                 (lambda (rule) (phony--external-name rule))
                 (phony--dependency-data-linear-extension dependency-data))))
    (rules
     . ,(seq-map #'phony-dragonfly--serialize-rule
                 (phony--get-rules)))))

(defun phony-dragonfly-export (dependency-data)
  (interactive (list (phony--analyze-grammar)))
  (with-temp-file (file-name-concat phony-output-directory "rules.json")
    (json-insert (phony-dragonfly--serialize-rules dependency-data))
    (json-pretty-print-buffer)))

(defun phony-dragonfly--backend-directory (&optional name)
  (expand-file-name
   (locate-user-emacs-file
    (if name
        (file-name-concat "phony" name)
      "phony"))))

(defun phony-dragonfly-install-backend ()
  (interactive)
  (when (or (not (interactive-p))
            (y-or-n-p "Install dragonfly+kaldi backend? (This might take a while) "))
    (mkdir (phony-dragonfly--backend-directory) t)
    (let ((default-directory (phony-dragonfly--backend-directory)))
      (mkdir "model" t)
      (when (or (not (file-exists-p "model/kaldi-active-grammar"))
                (y-or-n-p "Model already downloaded.  Redownload? "))
        (delete-file "model/kaldi-active-grammar.zip")
        (delete-directory "model/kaldi-active-grammar" t)
        (url-copy-file "https://github.com/daanzu/kaldi-active-grammar/releases/download/v3.1.0/kaldi_model_daanzu_20211030-biglm.zip"
                       "model/kaldi-active-grammar.zip")
        (dired-compress-file "model/kaldi-active-grammar.zip"))
      (call-process python-interpreter nil nil t "-m" "venv" "python-venv")
      (call-process (expand-file-name "python-venv/bin/python") nil nil t
                    "-m" "pip"
                    "install" "dragonfly2" "dragonfly2[kaldi]" "kaldi-active-grammar[g2p_en]" "g2p_en"))))

(defun phony-dragonfly-start-backend ()
  (interactive)
  (let ((default-directory (phony-dragonfly--backend-directory)))
    (mkdir "dragonfly" t)
    (if (executable-find "guix")
        (start-process "Dragonfly" "*Dragonfly*" "env"
                       "guix" "shell" "portaudio" "gcc-toolchain"
                       "--" (file-name-concat
                             (file-name-directory (locate-library "phony"))
                             "dragonfly/run-dragonfly-guix")
                       "python-venv/bin/activate"
                       "--datadir" phony-output-directory
                       "--model" "model/kaldi-active-grammar/kaldi_model")
      (start-process "Dragonfly" "*Dragonfly*"
                     "python-venv/bin/python"
                     (file-name-concat
                      (file-name-directory (locate-library "phony"))
                      "dragonfly/run-dragonfly")
                     "--datadir" phony-output-directory
                     "--model" "model/kaldi-active-grammar/kaldi_model"))
    (display-buffer "*Dragonfly*")))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
