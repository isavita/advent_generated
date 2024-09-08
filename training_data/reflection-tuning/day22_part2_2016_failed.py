import re
from collections import namedtuple

Node = namedtuple('Node', ['x', 'y', 'size', 'used', 'avail', 'use'])

def parse_input(input_data):
    nodes = []
    for line in input_data.strip().split('\n')[2:]:  # Skip header lines
        match = re.match(r'/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%', line)
        if match:
            x, y, size, used, avail, use = map(int, match.groups())
            nodes.append(Node(x, y, size, used, avail, use))
    return nodes

def count_viable_pairs(nodes):
    count = 0
    for a in nodes:
        for b in nodes:
            if a != b and a.used > 0 and a.used <= b.avail:
                count += 1
    return count

def find_empty_node(nodes):
    return next(node for node in nodes if node.used == 0)

def manhattan_distance(a, b):
    return abs(a.x - b.x) + abs(a.y - b.y)

def calculate_steps(nodes):
    max_x = max(node.x for node in nodes)
    max_y = max(node.y for node in nodes)
    goal_node = next(node for node in nodes if node.x == max_x and node.y == 0)
    empty_node = find_empty_node(nodes)

    # Steps to move empty node next to goal node
    steps = manhattan_distance(empty_node, Node(goal_node.x - 1, 0, 0, 0, 0, 0))

    # Steps to move goal data to (0, 0)
    steps += 5 * (goal_node.x - 1)  # 5 steps to move data one position left
    steps += 1  # Final step to move data to (0, 0)

    return steps

def solve(input_data):
    nodes = parse_input(input_data)
    part1 = count_viable_pairs(nodes)
    part2 = calculate_steps(nodes)
    return part1, part2

# Example usage:
# input_data = ...  # Your puzzle input here
# part1, part2 = solve(input_data)
# print(f"Part 1: {part1}")
# print(f"Part 2: {part2}")
