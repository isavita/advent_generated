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

def dfs(valve, time, bitmask, valves, tunnels, cache):
    if time <= 0:
        return 0
    
    key = (valve, time, bitmask)
    if key in cache:
        return cache[key]
    
    max_pressure = 0
    
    # Try opening the valve
    if valves[valve] > 0 and not (bitmask & (1 << ord(valve) - ord('A'))):
        new_bitmask = bitmask | (1 << ord(valve) - ord('A'))
        pressure = (time - 1) * valves[valve] + dfs(valve, time - 1, new_bitmask, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    # Try moving to connected valves
    for next_valve in tunnels[valve]:
        pressure = dfs(next_valve, time - 1, bitmask, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    cache[key] = max_pressure
    return max_pressure

def main():
    valves, tunnels = parse_input('input.txt')
    cache = {}
    max_pressure = dfs('AA', 30, 0, valves, tunnels, cache)
    print(max_pressure)

if __name__ == "__main__":
    main()
