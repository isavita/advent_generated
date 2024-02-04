
import heapq
import re
from collections import namedtuple

Point = namedtuple('Point', ['x', 'y'])

Neighbors4 = [Point(0, 1), Point(0, -1), Point(1, 0), Point(-1, 0)]
re_pattern = re.compile(r'-x(\d+)-y(\d+)')

class Node:
    def __init__(self, used, avail):
        self.used = used
        self.avail = avail

def main():
    nodes = {}
    with open("input.txt", "r") as file:
        input_lines = file.readlines()[2:]
        for line in input_lines:
            fields = line.split()
            matches = re_pattern.findall(fields[0])[0]
            p = Point(int(matches[0]), int(matches[1]))
            n = Node(int(fields[2][:-1]), int(fields[3][:-1]))
            nodes[p] = n
    print(minmoves(nodes))

def minmoves(nodes):
    w, _ = dim(nodes)
    goal = Point(w, 0)
    hole = find_hole(nodes)
    moves_sum = 0
    while goal != Point(0, 0):
        next_pos = Point(goal.x - 1, 0)
        m = moves(nodes, goal, hole, next_pos)
        moves_sum += m
        hole = next_pos
        m = moves(nodes, goal, goal, hole)
        moves_sum += m
        goal, hole = hole, goal
    return moves_sum

def find_hole(nodes):
    for p, n in nodes.items():
        if n.used == 0:
            return p
    raise ValueError("no hole")

def moves(nodes, goal, from_pos, to_pos):
    w, h = dim(nodes)
    depth = {from_pos: 0}
    pq = [(0, from_pos)]
    heapq.heapify(pq)
    while pq:
        curr_depth, p = heapq.heappop(pq)
        if p == to_pos:
            return curr_depth
        currdepth = depth[p] + 1
        for n in Neighbors4:
            next_pos = Point(p.x + n.x, p.y + n.y)
            if next_pos.x < 0 or next_pos.y < 0 or next_pos.x > w or next_pos.y > h or nodes[next_pos].used > 400 or next_pos == goal:
                continue
            if next_pos not in depth or currdepth < depth[next_pos]:
                depth[next_pos] = currdepth
                heapq.heappush(pq, (curr_depth + 1, next_pos))
    raise ValueError("no possible path")

def dim(nodes):
    w, h = 0, 0
    for p in nodes.keys():
        if p.x > w:
            w = p.x
        if p.y > h:
            h = p.y
    return w, h

main()
