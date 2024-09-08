import re

class Nanobot:
    def __init__(self, x, y, z, r):
        self.pos = (x, y, z)
        self.r = r

def manhattan_distance(pos1, pos2):
    return sum(abs(a - b) for a, b in zip(pos1, pos2))

def solve(filename):
    nanobots = []
    
    # Parse input file
    with open(filename, 'r') as file:
        for line in file:
            x, y, z, r = map(int, re.findall(r'-?\d+', line))
            nanobots.append(Nanobot(x, y, z, r))
    
    # Find strongest nanobot
    strongest = max(nanobots, key=lambda bot: bot.r)
    
    # Count nanobots in range
    in_range = sum(1 for bot in nanobots if manhattan_distance(strongest.pos, bot.pos) <= strongest.r)
    
    return in_range

print(solve('input.txt'))
