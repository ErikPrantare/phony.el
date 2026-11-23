;; -*- lexical-binding: t; -*-
(require 'phony-serialize-syntax)
(require 'benchmark)

(defun phony--run-benchmark ()
  (interactive)
  (let* ((phony--default-grammar
          (phony--read-syntax-from-file
           (expand-file-name
            "benchmark/big-grammar.el"
            (file-name-directory (locate-library "phony")))))
         (phony--exported-grammar (phony--finalize-grammar))
         (utterances '("bring air to boff"
                       "go up twice"
                       "swap air and boff and plex and yank outside with its outside outside")))
    ;; cold time benchmark
    (message "Cold time benchmark:")
    (dolist (utterance utterances)
      (message "%s: %s"
               utterance
               (benchmark-run 1 (phony-evaluate utterance))))
    ;; warm time benchmark
    (message "Warm time benchmark:")
    (dolist (utterance utterances)
      (message "%s: %s"
               utterance
               (benchmark-run 20 (phony-evaluate utterance))))
    ;; memory benchmark
    (message "Memory benchmark:")
    (let ((previous-memory-count (memory-use-counts)))
      (dolist (utterance utterances)
        (phony-evaluate utterance)
        (let ((memory-count (memory-use-counts)))
          (message "%s: %s"
                   utterance
                   (seq-mapn #'- memory-count previous-memory-count))
          (setq previous-memory-count memory-count))))))
