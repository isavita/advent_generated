
def find_position(mat, ch):
    for i in range(len(mat)):
        for j in range(len(mat[i])):
            if mat[i][j] == ch:
                return i, j
    return -1, -1

def ok(mat, st, seq):
    curr_i, curr_j = st
    for ch in seq:
        if mat[curr_i][curr_j] == ' ':
            return False
        if ch == '^':
            curr_i -= 1
        elif ch == 'v':
            curr_i += 1
        elif ch == '<':
            curr_j -= 1
        elif ch == '>':
            curr_j += 1
        if curr_i < 0 or curr_i >= len(mat) or curr_j < 0 or curr_j >= len(mat[0]):
            return False
    return True

def generate_moves(position, objective, pad):
    obj_i, obj_j = find_position(pad, objective)
    pos_i, pos_j = position
    ret = ""
    if pos_j > obj_j:
        ret += "<" * (pos_j - obj_j)
    if pos_i > obj_i:
        ret += "^" * (pos_i - obj_i)
    if pos_i < obj_i:
        ret += "v" * (obj_i - pos_i)
    if pos_j < obj_j:
        ret += ">" * (obj_j - pos_j)

    if not ok(pad, position, ret):
        ret = ""
        if pos_j < obj_j:
            ret += ">" * (obj_j - pos_j)
        if pos_i > obj_i:
            ret += "^" * (pos_i - obj_i)
        if pos_i < obj_i:
            ret += "v" * (obj_i - pos_i)
        if pos_j > obj_j:
            ret += "<" * (pos_j - obj_j)
    return ret

def solve(code, robots, key_pad, robot_pad, max_robots, memo):
    if robots <= 0:
        return len(code)
    
    state = (code, robots)
    if state in memo:
        return memo[state]

    ret = 0
    pos_i, pos_j = 3, 2
    if robots != max_robots:
        pos_i = 0

    for ch in code:
        if robots == max_robots:
            moves = generate_moves((pos_i, pos_j), ch, key_pad)
            pos_i, pos_j = find_position(key_pad, ch)
        else:
            moves = generate_moves((pos_i, pos_j), ch, robot_pad)
            pos_i, pos_j = find_position(robot_pad, ch)
        ret += solve(moves + "A", robots - 1, key_pad, robot_pad, max_robots, memo)
    
    memo[state] = ret
    return ret

with open("input.txt", "r") as f:
    content = f.read()

max_robots = 3
key_pad = ["789", "456", "123", " 0A"]
robot_pad = [" ^A", "<v>"]

ret = 0
codes = content.strip().split("\n")

for code in codes:
    code = code.strip()
    if not code:
        continue

    numeric_part = 0
    for char in code:
        if '0' <= char <= '9':
            numeric_part = numeric_part * 10 + int(char)

    memo = {}
    sv = solve(code, max_robots, key_pad, robot_pad, max_robots, memo)
    ret += sv * numeric_part

print(ret)
