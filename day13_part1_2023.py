
import sys

def parse_input(input):
    mirrors = []

    mirror_str = []
    for line in input:
        if line == "":
            mirrors.append(parse_mirror(mirror_str))
            mirror_str = []
        else:
            mirror_str.append(line)
    
    mirrors.append(parse_mirror(mirror_str))

    return mirrors

def parse_mirror(mirror_str):
    mirror = {
        "Rows": [0] * len(mirror_str),
        "Cols": [0] * len(mirror_str[0])
    }

    for y, line in enumerate(mirror_str):
        for x, char in enumerate(line):
            mirror["Rows"][y] <<= 1
            mirror["Cols"][x] <<= 1
            if char == '#':
                mirror["Rows"][y] += 1
                mirror["Cols"][x] += 1
    
    return mirror

def get_mirror_axis(lines):
    for i in range(1, len(lines)):
        is_mirror = True

        for j in range(min(i, len(lines)-i)):
            if lines[i-1-j] != lines[i+j]:
                is_mirror = False
        
        if is_mirror:
            return i
    
    return 0

def get_mirror_axis_with_one_smudge(lines):
    for i in range(1, len(lines)):
        is_mirror = True
        num_smudges = 0

        for j in range(min(i, len(lines)-i)):
            if lines[i-1-j] != lines[i+j]:
                if num_smudges > 0:
                    is_mirror = False
                else:
                    dif = lines[i-1-j] ^ lines[i+j]
                    is_only_one_smudge = (dif & (dif - 1)) == 0
                    if is_only_one_smudge:
                        num_smudges += 1
                    else:
                        is_mirror = False
        
        if is_mirror and num_smudges == 1:
            return i
    
    return 0

def solve(input):
    mirrors = parse_input(input)

    res = 0
    for mirror in mirrors:
        res += get_mirror_axis(mirror["Cols"])
        res += get_mirror_axis(mirror["Rows"]) * 100
    
    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read().strip().split('\n')

if __name__ == "__main__":
    input = read_file("input.txt")
    print(solve(input))
