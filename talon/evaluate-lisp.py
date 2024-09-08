from talon import Module, Context, actions
import subprocess

module = Module()

def evaluate_lisp_async(expression: str):
    return subprocess.Popen(
        ["emacsclient",
         "--eval",
         f"(with-current-buffer (window-buffer) {expression})"])

def evaluate_lisp(expression: str):
    evaluate_lisp_async(expression).wait()

@module.action_class
class Actions:
    def emacs_lisp(expression: str):
        "Run an elisp expression."
        evaluate_lisp(expression)

    def emacs_lisp_async(expression: str):
        "Run an elisp expression and don't wait for termination."
        evaluate_lisp_async(expression)
