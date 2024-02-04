
with open("input.txt", "r") as file:
    lengthsStr = file.readline().strip().split(",")
    lengths = [int(l) for l in lengthsStr]

list = [i for i in range(256)]
currentPosition = 0
skipSize = 0

for length in lengths:
    for i in range(length//2):
        start = (currentPosition + i) % 256
        end = (currentPosition + length - 1 - i) % 256
        list[start], list[end] = list[end], list[start]

    currentPosition = (currentPosition + length + skipSize) % 256
    skipSize += 1

result = list[0] * list[1]
print(result)
