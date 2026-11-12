;; -*- lexical-binding: t; -*-
(require 'phony-serialize-syntax)
(require 'phony-experimental)
(require 'benchmark)

(defun phony--run-benchmark ()
  (interactive)
  (let* ((phony--default-grammar
          (phony--read-syntax-from-file
           (expand-file-name
            "benchmark/big-grammar.el"
            (file-name-directory (locate-library "phony")))))
         (phony--exported-grammar
          (phony--prepare-grammar phony--default-grammar))
         (utterances '("bring air to boff"
                       "go up twice"
                       "swap air and boff and plex and yank outside to its outside outside")))
    ;; warm up
    (dolist (utterance utterances)
      (phony-experimental-evaluate utterance))
    ;; time benchmark
    (message "Time benchmark:")
    (dolist (utterance utterances)
      (message "%s: %s"
               utterance
               (benchmark-run 20
                 (phony-experimental-evaluate utterance))))
    ;; memory benchmark
    (message "Memory benchmark:")
    (let ((previous-memory-count (memory-use-counts)))
      (dolist (utterance utterances)
        (phony-experimental-evaluate utterance)
        (let ((memory-count (memory-use-counts)))
          (message "%s: %s"
                   utterance
                   (seq-mapn #'- memory-count previous-memory-count))
          (setq previous-memory-count memory-count))))))
