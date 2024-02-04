
import re

class Node:
    def __init__(self, used, avail):
        self.used = used
        self.avail = avail

def read_nodes(filename):
    nodes = []
    with open(filename, 'r') as file:
        node_regex = re.compile(r'node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%')
        for line in file:
            matches = node_regex.search(line)
            if matches:
                used = int(matches.group(1))
                avail = int(matches.group(2))
                nodes.append(Node(used, avail))
    return nodes

def count_viable_pairs(nodes):
    count = 0
    for i, a in enumerate(nodes):
        for j, b in enumerate(nodes):
            if i != j and a.used > 0 and a.used <= b.avail:
                count += 1
    return count

nodes = read_nodes("input.txt")
viable_pairs = count_viable_pairs(nodes)
print(viable_pairs)
