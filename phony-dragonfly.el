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

(cl-defgeneric phony-dragonfly--serialize-element (element)
  `((type . "undefined")))

(cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-literal))
  `((type . "literal")
    (utterance . ,(phony--element-literal-string literal))))

(cl-defmethod phony-dragonfly--serialize-element ((variable phony--element-argument))
  `((type . "argument")
    (name . ,(symbol-name
              (phony--element-argument-name variable)))
    (rule . ,(phony-dragonfly--serialize-element
              (phony--element-argument-form variable)))))

(cl-defmethod phony-dragonfly--serialize-element ((dictionary phony--element-rule))
  `((type . "rule")
    (name . ,(phony--external-name
              (phony--element-rule-name dictionary)))))

(cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-one-or-more))
  `((type . "one-or-more")
    (element . ,(phony-dragonfly--serialize-element
                 (phony--element-one-or-more-element literal))))

  (cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-zero-or-more))
    `((type . "zero-or-more")
      (element . ,(phony-dragonfly--serialize-element
                   (phony--element-zero-or-more-element literal))))))

(cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-optional))
  `((type . "optional")
    (element . ,(phony-dragonfly--serialize-element
                 (phony--element-optional-element literal)))))

(cl-defmethod phony-dragonfly--serialize-element ((literal phony--element-external-rule))
  `((type . "impossible")))

(cl-defmethod phony-dragonfly--serialize-element ((sequence phony--element-sequence))
  `((type . "sequence")
    (elements . ,(seq-into (seq-map #'phony-dragonfly--serialize-element sequence)
                           'vector))))

(cl-defgeneric phony-dragonfly--serialize-rule-concrete (rule))

(cl-defmethod phony-dragonfly--serialize-rule-concrete ((rule phony--procedure-rule))
  `((type . "procedure-definition")
    (name . ,(phony--external-name rule))
    (function . ,(symbol-name (phony--procedure-rule-function rule)))
    (element . ,(phony-dragonfly--serialize-element
                 (phony--procedure-rule-element rule)))
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

(defun phony-dragonfly--serialize-rules (analysis-data)
  `((dependency-linear-extension
     . ,(apply #'vector
                (seq-map
                 (lambda (rule) (phony--external-name rule))
                 (phony--analysis-data-linear-extension analysis-data))))
    (rules
     . ,(seq-map #'phony-dragonfly--serialize-rule
                 (phony--get-rules)))))

(defun phony-dragonfly-export (analysis-data)
  (interactive (list (phony--analyze-grammar)))
  (with-temp-file (file-name-concat phony-output-directory "rules.json")
    (json-insert (phony-dragonfly--serialize-rules analysis-data))
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
      (require 'python)
      (message "Creating python virtual environment...")
      (call-process python-interpreter nil nil t "-m" "venv" "python-venv")
      (call-process (expand-file-name "python-venv/bin/python") nil nil t
                    "-m" "pip"
                    "install" "dragonfly2" "dragonfly2[kaldi]" "kaldi-active-grammar[g2p_en]" "g2p_en")
      (message "Creating python virtual environment...done")

      (mkdir "model" t)
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
                       "--datadir" phony-output-directory
                       "--model" "model/kaldi-active-grammar/kaldi_model")
      (start-process "Dragonfly" "*Dragonfly*"
                     (file-name-concat
                      (file-name-directory (locate-library "phony"))
                      "dragonfly/run-phony-dragonfly")
                       "python-venv/bin/activate"
                       "--datadir" phony-output-directory
                       "--model" "model/kaldi-active-grammar/kaldi_model"))
    (display-buffer "*Dragonfly*")))

(provide 'phony-dragonfly)
;;; phony-dragonfly.el ends here
