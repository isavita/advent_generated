
with open("input.txt", "r") as file:
    count = 0
    for line in file:
        parts = line.strip().split(" | ")
        output = parts[1]
        for digit in output.split(" "):
            if len(digit) in [2, 4, 3, 7]:
                count += 1

print(count)
