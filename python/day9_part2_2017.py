with open("input.txt", "r") as file:
    data = file.read().strip()

score = 0
garbage_count = 0
in_garbage = False
ignore_next = False

for char in data:
    if ignore_next:
        ignore_next = False
    elif char == "!":
        ignore_next = True
    elif in_garbage:
        if char == ">":
            in_garbage = False
        else:
            garbage_count += 1
    elif char == "<":
        in_garbage = True
    elif char == "{":
        score += 1
    elif char == "}":
        pass

print(score)
print(garbage_count)