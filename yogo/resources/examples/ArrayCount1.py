count = 0
f = open("some-file", "w")
for i in range(len(arr)):
    f.write("Element: ", arr[i])
    if (arr[i] == k):
        count += 1
count = arr.count(k)