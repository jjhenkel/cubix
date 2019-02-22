#!/usr/bin/python3

import sys
import os

lang = { "java": "java", "py": "python" }

transform, path = sys.argv[1], sys.argv[2]
ext = path.split(".")[-1]

os.system("stack exec examples-multi {lang} {transform} {path}".format(
    lang=lang[ext], transform=transform, path=path))
