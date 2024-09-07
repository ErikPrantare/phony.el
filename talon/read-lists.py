from talon import Context, Module
import talon, json, os.path

module = Module()
context = Context()

def load_lists(path, dummy_argument):
    with open(path, 'r') as inn:
        message = json.load(inn)
        for list_name in message:
            module.list(list_name, "")
            context.lists["user." + list_name] = message[list_name]

path = os.path.expanduser("~/.talon/emacs-lists.json")
load_lists(path, None)
talon.fs.watch(path, load_lists)
