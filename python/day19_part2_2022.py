from collections import deque

class Blueprint:
    def __init__(self, id, ore_cost, clay_ore_cost, obsidian_ore_cost, obsidian_clay_cost, geode_ore_cost, geode_obsidian_cost):
        self.id = id
        self.ore_cost = ore_cost
        self.clay_ore_cost = clay_ore_cost
        self.obsidian_ore_cost = obsidian_ore_cost
        self.obsidian_clay_cost = obsidian_clay_cost
        self.geode_ore_cost = geode_ore_cost
        self.geode_obsidian_cost = geode_obsidian_cost

class State:
    def __init__(self, ore=0, clay=0, obsidian=0, geode=0, ore_robots=0, clay_robots=0, obsidian_robots=0, geode_robots=0, time_left=0):
        self.ore = ore
        self.clay = clay
        self.obsidian = obsidian
        self.geode = geode
        self.ore_robots = ore_robots
        self.clay_robots = clay_robots
        self.obsidian_robots = obsidian_robots
        self.geode_robots = geode_robots
        self.time_left = time_left

def max_geode(b, st):
    max_geodes = 0
    q = deque([st])
    visited = set()
    while q:
        s = q.popleft()
        max_geodes = max(max_geodes, s.geode)
        if s.time_left == 0:
            continue
        o = max(b.ore_cost, b.clay_ore_cost, b.obsidian_ore_cost, b.geode_ore_cost)
        if s.ore_robots >= o:
            s.ore_robots = o
        if s.clay_robots >= b.obsidian_clay_cost:
            s.clay_robots = b.obsidian_clay_cost
        if s.obsidian_robots >= b.geode_obsidian_cost:
            s.obsidian_robots = b.geode_obsidian_cost
        max_ore = s.time_left * o - s.ore_robots * (s.time_left - 1)
        if s.ore >= max_ore:
            s.ore = max_ore
        max_clay = s.time_left * b.obsidian_clay_cost - s.clay_robots * (s.time_left - 1)
        if s.clay >= max_clay:
            s.clay = max_clay
        max_obsidian = s.time_left * b.geode_obsidian_cost - s.obsidian_robots * (s.time_left - 1)
        if s.obsidian >= max_obsidian:
            s.obsidian = max_obsidian
        state_tuple = (s.ore, s.clay, s.obsidian, s.geode, s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots, s.time_left)
        if state_tuple in visited:
            continue
        visited.add(state_tuple)
        q.append(State(s.ore + s.ore_robots, s.clay + s.clay_robots, s.obsidian + s.obsidian_robots, s.geode + s.geode_robots,
                       s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots, s.time_left - 1))
        if s.ore >= b.ore_cost:
            q.append(State(s.ore - b.ore_cost + s.ore_robots, s.clay + s.clay_robots, s.obsidian + s.obsidian_robots, s.geode + s.geode_robots,
                           s.ore_robots + 1, s.clay_robots, s.obsidian_robots, s.geode_robots, s.time_left - 1))
        if s.ore >= b.clay_ore_cost:
            q.append(State(s.ore - b.clay_ore_cost + s.ore_robots, s.clay + s.clay_robots, s.obsidian + s.obsidian_robots, s.geode + s.geode_robots,
                           s.ore_robots, s.clay_robots + 1, s.obsidian_robots, s.geode_robots, s.time_left - 1))
        if s.ore >= b.obsidian_ore_cost and s.clay >= b.obsidian_clay_cost:
            q.append(State(s.ore - b.obsidian_ore_cost + s.ore_robots, s.clay - b.obsidian_clay_cost + s.clay_robots, s.obsidian + s.obsidian_robots, s.geode + s.geode_robots,
                           s.ore_robots, s.clay_robots, s.obsidian_robots + 1, s.geode_robots, s.time_left - 1))
        if s.ore >= b.geode_ore_cost and s.obsidian >= b.geode_obsidian_cost:
            q.append(State(s.ore - b.geode_ore_cost + s.ore_robots, s.clay + s.clay_robots, s.obsidian - b.geode_obsidian_cost + s.obsidian_robots, s.geode + s.geode_robots,
                           s.ore_robots, s.clay_robots, s.obsidian_robots, s.geode_robots + 1, s.time_left - 1))
    return max_geodes

blueprints = []
with open("input.txt") as file:
    for line in file:
        parts = line.strip().split()
        id = int(parts[1][:-1])
        ore_cost = int(parts[6])
        clay_ore_cost = int(parts[12])
        obsidian_ore_cost = int(parts[18])
        obsidian_clay_cost = int(parts[21])
        geode_ore_cost = int(parts[27])
        geode_obsidian_cost = int(parts[30])
        blueprints.append(Blueprint(id, ore_cost, clay_ore_cost, obsidian_ore_cost, obsidian_clay_cost, geode_ore_cost, geode_obsidian_cost))

init = State(ore_robots=1, time_left=32)
prod = 1
for b in blueprints[:3]:
    prod *= max_geode(b, init)
print(prod)