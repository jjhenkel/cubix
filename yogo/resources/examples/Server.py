x = input()
port = 0
if not x:
    port = 3000
else:
    port = x
serve(port)

### These also work:

# x = input()
# port = 3000
# if x:
#     port = x
# serve(port)

# port = input()
# if not port:
#     port = 3000
# serve(port)
