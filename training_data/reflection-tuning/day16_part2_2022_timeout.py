import re
from collections import defaultdict

def parse_input(filename):
    valves = {}
    tunnels = {}
    with open(filename, 'r') as f:
        for line in f:
            valve, flow_rate, leads_to = re.match(r'Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)', line).groups()
            valves[valve] = int(flow_rate)
            tunnels[valve] = leads_to.split(', ')
    return valves, tunnels

def dfs(valve, time, opened, valves, tunnels, cache):
    if time <= 0:
        return 0
    
    state = (valve, time, opened)
    if state in cache:
        return cache[state]
    
    max_pressure = 0
    
    # Try opening the current valve
    if valves[valve] > 0 and not (opened & (1 << ord(valve[0]) - ord('A'))):
        new_opened = opened | (1 << ord(valve[0]) - ord('A'))
        pressure = (time - 1) * valves[valve] + dfs(valve, time - 1, new_opened, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    # Try moving to connected valves
    for next_valve in tunnels[valve]:
        pressure = dfs(next_valve, time - 1, opened, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    cache[state] = max_pressure
    return max_pressure

def dfs_with_elephant(valve1, valve2, time, opened, valves, tunnels, cache):
    if time <= 0:
        return 0
    
    state = (valve1, valve2, time, opened)
    if state in cache:
        return cache[state]
    
    max_pressure = 0
    
    # All possible actions for you and the elephant
    actions1 = [(valve1, 0)] + [(v, 1) for v in tunnels[valve1]]
    actions2 = [(valve2, 0)] + [(v, 1) for v in tunnels[valve2]]
    
    for a1, cost1 in actions1:
        for a2, cost2 in actions2:
            if a1 == a2 and a1 != valve1:  # Avoid both moving to the same new valve
                continue
            
            new_opened = opened
            new_time = time - 1
            pressure = 0
            
            # Your action
            if cost1 == 0 and valves[a1] > 0 and not (opened & (1 << ord(a1[0]) - ord('A'))):
                new_opened |= (1 << ord(a1[0]) - ord('A'))
                pressure += valves[a1] * new_time
            
            # Elephant's action
            if cost2 == 0 and valves[a2] > 0 and not (new_opened & (1 << ord(a2[0]) - ord('A'))):
                new_opened |= (1 << ord(a2[0]) - ord('A'))
                pressure += valves[a2] * new_time
            
            pressure += dfs_with_elephant(a1, a2, new_time, new_opened, valves, tunnels, cache)
            max_pressure = max(max_pressure, pressure)
    
    cache[state] = max_pressure
    return max_pressure

def solve(filename):
    valves, tunnels = parse_input(filename)
    
    # Part 1
    cache = {}
    part1 = dfs('AA', 30, 0, valves, tunnels, cache)
    
    # Part 2
    cache = {}
    part2 = dfs_with_elephant('AA', 'AA', 26, 0, valves, tunnels, cache)
    
    return part1, part2

part1, part2 = solve('input.txt')
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
