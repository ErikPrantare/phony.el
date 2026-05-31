;;; phony-dragonfly.el ---                           -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026  Erik Präntare

;; Author: Erik Präntare <erik@system2>
;; Keywords: convenience
;; Created: 26 Jul 2025

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

(require 'phony)
(require 'dired-aux)
(require 'json)
(require 'python)

(cl-defgeneric phony-dragonfly--serialize-element (_element)
  "Convert ELEMENT to a form serializable by `json-encode'."
  ;; TODO: Document structure.
  `((type . "undefined")))

(cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-literal))
  "Convert LITERAL to a form serializable by `json-encode'."
  `((type . "literal")
    (utterance . ,(phony--element-literal-string literal))))

(cl-defmethod phony-dragonfly--serialize-element ((argument phony--element-argument))
  "Convert ARGUMENT to a form serializable by `json-encode'."
  `((type . "argument")
    (name . ,(symbol-name
              (phony--element-argument-name argument)))
    (rule . ,(phony-dragonfly--serialize-element
              (phony--element-argument-element argument)))))

(cl-defmethod phony-dragonfly--serialize-element ((dictionary phony--element-rule))
  "Convert DICTIONARY to a form serializable by `json-encode'."
  `((type . "rule")
    (name . ,(phony--external-name
              (phony--element-rule-name dictionary)))))

(cl-defmethod phony-dragonfly--serialize-element ((element phony--element-one-or-more))
  "Convert ELEMENT to a form serializable by `json-encode'."
  `((type . "one-or-more")
    (element . ,(phony-dragonfly--serialize-element
                 (phony--element-one-or-more-element element)))))

(cl-defmethod phony-dragonfly--serialize-element ((element phony--element-zero-or-more))
  "Convert ELEMENT to a form serializable by `json-encode'."
  `((type . "zero-or-more")
    (element . ,(phony-dragonfly--serialize-element
                 (phony--element-zero-or-more-element element)))))

(cl-defmethod phony-dragonfly--serialize-element ((element phony--element-optional))
  "Convert ELEMENT to a form serializable by `json-encode'."
  `((type . "optional")
    (element . ,(phony-dragonfly--serialize-element
                 (phony--element-optional-element element)))))

(cl-defmethod phony-dragonfly--serialize-element ((_element phony--element-external-rule))
  "Convert ELEMENT to a form serializable by `json-encode'."
  `((type . "impossible")))

(cl-defmethod phony-dragonfly--serialize-element ((element phony--element-sequence))
  "Convert ELEMENT to a form serializable by `json-encode'."
  `((type . "sequence")
    (elements . ,(seq-into
                  (seq-map
                   #'phony-dragonfly--serialize-element
                   (phony--element-sequence-elements element))
                  'vector))))

(cl-defgeneric phony-dragonfly--serialize-rule-definition (rule)
  "Convert RULE definition to a form serializable by `json-encode'.")

(cl-defmethod phony-dragonfly--serialize-rule-definition ((rule phony--procedure-rule))
  "Convert RULE definition to a form serializable by `json-encode'."
  `((type . "procedure")
    (name . ,(phony--external-name rule))
    (function . ,(symbol-name (phony--procedure-rule-function rule)))
    (element . ,(phony-dragonfly--serialize-element
                 (phony--procedure-rule-element rule)))
    (argument-list . ,(seq-into
                       (seq-map #'symbol-name
                                (phony--procedure-rule-arglist rule))
                       'vector))
    (export . ,(if (phony--procedure-rule-interactive-p rule) t :false))))

(cl-defmethod phony-dragonfly--serialize-rule-definition ((rule phony--open-rule))
  "Convert RULE definition to a form serializable by `json-encode'."
  `((type . "open")
    (name . ,(phony--external-name rule))
    (alternatives . ,(seq-into (seq-map #'phony--external-name
                                        (phony--open-rule-alternatives rule))
                               'vector))))

(cl-defmethod phony-dragonfly--serialize-rule-definition ((rule phony--dictionary))
  "Convert RULE to a form serializable by `json-encode'."
  `((type . "dictionary")
    (name . ,(phony--external-name rule))))

(defun phony-dragonfly--serialize-rule (rule)
  "Convert RULE to a form serializable by `json-encode'."
  `(,(make-symbol (phony--external-name rule))
    . ,(phony-dragonfly--serialize-rule-definition rule)))

(defun phony-dragonfly--serialize-rules (analysis-data)
  ;; checkdoc-params: (analysis-data)
  "Convert the current grammar to a form serializable by `json-encode'."
  `((dependency-linear-extension
     . ,(apply #'vector
               (seq-map
                (lambda (rule) (phony--external-name rule))
                (phony--analysis-data-linear-extension analysis-data))))
    (rules
     . ,(seq-map #'phony-dragonfly--serialize-rule
                 (phony--get-rules)))))

(defun phony-dragonfly-export (analysis-data)
  ;; checkdoc-params: (analysis-data)
  "Export the current grammar to the dragonfly backend."
  (interactive (list (phony--analyze-grammar)))
  (mkdir (phony-dragonfly--backend-directory) t)
  (with-temp-file (phony-dragonfly--backend-directory "rules.json")
    (json-insert (phony-dragonfly--serialize-rules analysis-data))
    (json-pretty-print-buffer)))

(defun phony-dragonfly--backend-directory (&optional name)
  "Return path to file NAME in phony's output directory for dragonfly."
  (phony--output-directory "dragonfly" name))

(defun phony-dragonfly-install-backend ()
  "Install the dragonfly backend."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (y-or-n-p "Install dragonfly+kaldi backend (this might take a while)? "))
    (mkdir (phony-dragonfly--backend-directory) t)
    (let ((default-directory (phony-dragonfly--backend-directory)))
      (message "Creating python virtual environment...")
      (call-process python-interpreter nil nil t "-m" "venv" "python-venv")
      (call-process (expand-file-name "python-venv/bin/python") nil nil t
                    "-m" "pip"
                    "install" "dragonfly2" "dragonfly2[kaldi]" "kaldi-active-grammar[g2p_en]" "g2p_en")
      (message "Creating python virtual environment...done")

      (mkdir "model" t)
      (make-symbolic-link (phony--output-directory "dictionaries.json")
                          "dictionaries.json" t)
      (when (or (not (file-exists-p "model/kaldi-active-grammar"))
                (y-or-n-p "Model already downloaded.  Redownload? "))
        (delete-file "model/kaldi-active-grammar.zip")
        (delete-directory "model/kaldi-active-grammar" t)
        (message "Downloading model..." )
        (switch-to-buffer "*Phony Dragonfly model download*")
        (make-process
         :name "curl-kaldi-model"
         :buffer "*Phony Dragonfly model download*"
         :command (list "curl"
                        "-L" "-o" "model/kaldi-active-grammar.zip"
                        "https://github.com/daanzu/kaldi-active-grammar/releases/download/v3.1.0/kaldi_model_daanzu_20211030-biglm.zip")
         :filter (lambda (process output)
                   (with-current-buffer (process-buffer process)
                     (save-excursion
                       (goto-char (process-mark process))
                       (insert (string-replace "" "\n" output))
                       (move-marker (process-mark process) (point)))))
         :sentinel (lambda (process sentinel)
                     (when (equal sentinel "finished\n")
                       (message "Downloading model...done")
                       (message "Decompressing model...")
                       (with-current-buffer (process-buffer process)
                         (dired-compress-file "model/kaldi-active-grammar.zip"))
                       (message "Decompressing model...done")
                       (kill-buffer "*Phony Dragonfly model download*"))))))))

(defun phony-dragonfly-start-backend ()
  "Start the dragonfly backend."
  (interactive)
  (let ((default-directory (phony-dragonfly--backend-directory)))
    (mkdir "dragonfly" t)
    (if (executable-find "guix")
        (start-process "Dragonfly" "*Dragonfly*"
                       "guix" "shell" "portaudio" "gcc-toolchain"
                       "--" (file-name-concat
                             (file-name-directory (locate-library "phony"))
                             "dragonfly/run-phony-dragonfly-guix")
                       "python-venv/bin/activate"
                       "--datadir" (phony-dragonfly--backend-directory)
                       "--model" "model/kaldi-active-grammar/kaldi_model")
      (start-process "Dragonfly" "*Dragonfly*"
                     (file-name-concat
                      (file-name-directory (locate-library "phony"))
                      "dragonfly/run-phony-dragonfly")
                       "python-venv/bin/activate"
                       "--datadir" (phony-dragonfly--backend-directory)
                       "--model" "model/kaldi-active-grammar/kaldi_model"))
    (display-buffer "*Dragonfly*")))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
