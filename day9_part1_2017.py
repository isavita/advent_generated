
def calculate_score(stream):
    total_score = 0
    current_score = 0
    in_garbage = False
    ignore_next = False

    for char in stream:
        if ignore_next:
            ignore_next = False
        elif char == "!":
            ignore_next = True
        elif char == "<" and not in_garbage:
            in_garbage = True
        elif char == ">" and in_garbage:
            in_garbage = False
        elif char == "{" and not in_garbage:
            current_score += 1
        elif char == "}" and not in_garbage:
            total_score += current_score
            current_score -= 1

    return total_score

with open("input.txt", "r") as file:
    stream = file.read().strip()

print(calculate_score(stream))
