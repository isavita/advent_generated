with open('input.txt', 'r') as file:
    lines = file.readlines()

illegal_chars = {')': 3, ']': 57, '}': 1197, '>': 25137}
total_score = 0

for line in lines:
    stack = []
    for i, char in enumerate(line):
        if char in '([{<':
            stack.append(char)
        elif char in ')]}>':
            if not stack or ord(char) - ord(stack.pop()) not in [1, 2]:
                total_score += illegal_chars[char]
                break

print(total_score)