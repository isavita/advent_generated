import strutils

var file = open("input.txt")
let lines = file.readAll().splitLines

var score = 0
var stack: seq[char]

for line in lines:
    stack = @[]
    for c in line:
        if c in "([{<":
            stack.add(c)
        else:
            if stack.len == 0:
                continue
            let last = stack[^1]
            stack.delete(stack.high)
            if (c == ')' and last != '(') or
               (c == ']' and last != '[') or
               (c == '}' and last != '{') or
               (c == '>' and last != '<'):
                case c
                of ')': score += 3
                of ']': score += 57
                of '}': score += 1197
                of '>': score += 25137
                else: discard
                break

echo score