#!/usr/bin/python3

import os
import sys

path = sys.argv[1]

os.system("./cubix.py debug " + path + " | ./extract-ast.py | ./prettyprint-sexp.rkt | less")
