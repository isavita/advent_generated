
import sys

def check_and_complete_line(line):
    pairings = {')': '(', ']': '[', '}': '{', '>': '<'}
    score_values = {')': 1, ']': 2, '}': 3, '>': 4}
    opening = "([{<"
    closing = ")]}>"
    stack = []

    for char in line:
        if char in opening:
            stack.append(char)
        elif char in closing:
            if len(stack) == 0 or stack[-1] != pairings[char]:
                return 0, False
            stack.pop()

    if len(stack) == 0:
        return 0, False

    score = 0
    for i in range(len(stack) - 1, -1, -1):
        score *= 5
        score += score_values[get_closing_char(stack[i])]
    return score, True

def get_closing_char(opening_char):
    if opening_char == '(':
        return ')'
    elif opening_char == '[':
        return ']'
    elif opening_char == '{':
        return '}'
    elif opening_char == '<':
        return '>'
    else:
        return ' '

if __name__ == "__main__":
    scores = []

    with open("input.txt", "r") as file:
        for line in file:
            line = line.strip()
            score, incomplete = check_and_complete_line(line)
            if incomplete:
                scores.append(score)

    middle_score = sorted(scores)[len(scores) // 2]
    print(middle_score)
