
with open("input.txt", "r") as file:
    input = file.readline().strip()

repeatedInput = [int(digit) for digit in input * 10000]
offset = int(input[:7])

for _ in range(100):
    total = 0
    for i in range(len(repeatedInput) - 1, offset - 1, -1):
        total += repeatedInput[i]
        repeatedInput[i] = total % 10

result = "".join(map(str, repeatedInput[offset:offset + 8]))
print(result)
