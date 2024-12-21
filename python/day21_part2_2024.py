
def find_position(mat, ch):
    for i in range(len(mat)):
        for j in range(len(mat[i])):
            if mat[i][j] == ch:
                return i, j
    return -1, -1

def ok(mat, st, seq):
    curr_i, curr_j = st
    for ch in seq:
        if not (0 <= curr_i < len(mat) and 0 <= curr_j < len(mat[curr_i])) or mat[curr_i][curr_j] == ' ':
            return False
        if ch == '^':
            curr_i -= 1
        elif ch == 'v':
            curr_i += 1
        elif ch == '<':
            curr_j -= 1
        elif ch == '>':
            curr_j += 1
    return True

def generate_moves(position, objective, pad):
    obj_pos_i, obj_pos_j = find_position(pad, objective)
    pos_i, pos_j = position
    
    result = ""
    if pos_j > obj_pos_j:
        result += "<" * (pos_j - obj_pos_j)
    if pos_i > obj_pos_i:
        result += "^" * (pos_i - obj_pos_i)
    if pos_i < obj_pos_i:
        result += "v" * (obj_pos_i - pos_i)
    if pos_j < obj_pos_j:
        result += ">" * (obj_pos_j - pos_j)

    if not ok(pad, position, result):
        result = ""
        if pos_j < obj_pos_j:
            result += ">" * (obj_pos_j - pos_j)
        if pos_i > obj_pos_i:
            result += "^" * (pos_i - obj_pos_i)
        if pos_i < obj_pos_i:
            result += "v" * (obj_pos_i - pos_i)
        if pos_j > obj_pos_j:
            result += "<" * (pos_j - obj_pos_j)
            
    return result

def solve(code, robots, key_pad, robot_pad, max_robots, memo={}):
    key = (code, robots, max_robots)
    if key in memo:
        return memo[key]

    if robots <= 0:
        return len(code)

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

    memo[key] = ret
    return ret

with open("input.txt", "r") as f:
    content = f.read().strip()

max_robots = 26
key_pad = [
    "789",
    "456",
    "123",
    " 0A",
]
robot_pad = [
    " ^A",
    "<v>",
]

ret = 0
codes = content.split("\n")

for code in codes:
    code = code.strip()
    if not code:
        continue

    numeric_part = 0
    for char in code:
        if '0' <= char <= '9':
            numeric_part = numeric_part * 10 + int(char)

    ret += solve(code, max_robots, key_pad, robot_pad, max_robots) * numeric_part

print(ret)
