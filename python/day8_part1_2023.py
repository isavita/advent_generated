import re

ElemToMatch = "ZZZ"

with open("input.txt", "r") as file:
    lines = [line.strip() for line in file.readlines()]

instructions = lines[0]
desert_map = {}

for line in lines[2:]:
    if line:
        matches = re.findall(r"[A-Z]{3}", line)
        desert_map[matches[0]] = {"left": matches[1], "right": matches[2]}

current = "AAA"
steps = 0

while current != ElemToMatch:
    for direction in instructions:
        if direction == "R":
            current = desert_map[current]["right"]
        elif direction == "L":
            current = desert_map[current]["left"]
        steps += 1
        if current == ElemToMatch:
            break

print(steps)