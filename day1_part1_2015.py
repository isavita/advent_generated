with open("input.txt", "r") as file:
    data = file.read()

floor = 0
for char in data:
    if char == "(":
        floor += 1
    elif char == ")":
        floor -= 1

print(floor)