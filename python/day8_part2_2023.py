
import math

def parse_input(input):
    instructions = input[0]

    nodes = {}
    for line in input[2:]:
        head, children = parse_line(line)
        nodes[head] = children

    return instructions, nodes

def parse_line(line):
    parts = line.split(" = ")
    head = parts[0]
    children_trim = parts[1][1:-1]
    children_parts = children_trim.split(", ")
    children = (children_parts[0], children_parts[1])

    return head, children

def gcd(a, b):
    while b != 0:
        a, b = b, a % b
    return a

def lcm(a, b):
    return (a * b) // gcd(a, b)

def lcm_list(nums):
    if len(nums) == 0:
        return 0

    res = nums[0]
    for i in range(1, len(nums)):
        res = lcm(res, nums[i])

    return res

def solve(input):
    instructions, nodes = parse_input(input)

    starts = [node for node in nodes if node[-1] == 'A']
    steps = [0] * len(starts)
    instructions_length = len(instructions)

    for i in range(len(starts)):
        element = starts[i]
        while element[-1] != 'Z':
            instruction = instructions[steps[i] % instructions_length]
            if instruction == 'L':
                element = nodes[element][0]
            else:
                element = nodes[element][1]
            steps[i] += 1

    res = lcm_list(steps)
    return res

def read_file(file_name):
    with open(file_name, 'r') as file:
        return file.read().splitlines()

if __name__ == "__main__":
    input_data = read_file("input.txt")
    print(solve(input_data))
