#!/usr/bin/python3

while True:
    line = input()
    if line.count("Basic blocks"):
        break

while True:
    labels = input()
    if labels.count("Label") == 0:
        break
    ast = input()
    print(ast
          .replace("\'\"\'", "quote")
          .replace("'", '"'))
