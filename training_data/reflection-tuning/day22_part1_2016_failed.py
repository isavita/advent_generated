import re

def parse_node(line):
    pattern = r'/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%'
    match = re.match(pattern, line)
    if match:
        x, y, size, used, avail, use_percent = map(int, match.groups())
        return (x, y, size, used, avail)
    return None

def count_viable_pairs(nodes):
    count = 0
    for a in nodes:
        for b in nodes:
            if a != b and a[3] > 0 and a[3] <= b[4]:
                count += 1
    return count

def solve(input_data):
    lines = input_data.strip().split('\n')[2:]  # Skip the first two lines (header)
    nodes = [parse_node(line) for line in lines if parse_node(line)]
    
    return count_viable_pairs(nodes)

# Example usage:
# input_data = """
# Filesystem            Size  Used  Avail  Use%
# /dev/grid/node-x0-y0   10T    8T     2T   80%
# /dev/grid/node-x0-y1   11T    6T     5T   54%
# /dev/grid/node-x0-y2   32T   28T     4T   87%
# /dev/grid/node-x1-y0    9T    7T     2T   77%
# /dev/grid/node-x1-y1    8T    0T     8T    0%
# /dev/grid/node-x1-y2   11T    7T     4T   63%
# """
# result = solve(input_data)
# print(result)
