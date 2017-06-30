import importlib, sys
from inspect import getmembers, isfunction, getmodule
name = sys.argv[1]
importlib.import_module(name, package=None)
module = sys.modules[name]
the_funcs = getmembers(module, isfunction)
for item in the_funcs:
    try_module = getmodule(item[1])
    if try_module == module:
        print(item[0])
