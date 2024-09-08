from collections import defaultdict
import re

def parse_input(input_data):
    valves = {}
    for line in input_data.strip().split('\n'):
        valve, flow_rate, tunnels = re.match(r'Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)', line).groups()
        valves[valve] = {'flow': int(flow_rate), 'tunnels': tunnels.split(', ')}
    return valves

def floyd_warshall(valves):
    dist = defaultdict(lambda: float('inf'))
    for valve in valves:
        dist[valve, valve] = 0
        for neighbor in valves[valve]['tunnels']:
            dist[valve, neighbor] = 1
    
    for k in valves:
        for i in valves:
            for j in valves:
                dist[i, j] = min(dist[i, j], dist[i, k] + dist[k, j])
    return dist

def dfs(valve, time, bitmask, distances, flows, cache):
    if (valve, time, bitmask) in cache:
        return cache[valve, time, bitmask]
    
    max_pressure = 0
    for next_valve, flow in flows.items():
        bit = 1 << flows.index(next_valve)
        if bitmask & bit: continue
        time_left = time - distances[valve, next_valve] - 1
        if time_left <= 0: continue
        pressure = flow * time_left + dfs(next_valve, time_left, bitmask | bit, distances, flows, cache)
        max_pressure = max(max_pressure, pressure)
    
    cache[valve, time, bitmask] = max_pressure
    return max_pressure

def solve_part1(valves):
    distances = floyd_warshall(valves)
    flows = [(v, data['flow']) for v, data in valves.items() if data['flow'] > 0]
    flows.sort(key=lambda x: x[1], reverse=True)
    return dfs('AA', 30, 0, distances, flows, {})

def solve_part2(valves):
    distances = floyd_warshall(valves)
    flows = [(v, data['flow']) for v, data in valves.items() if data['flow'] > 0]
    flows.sort(key=lambda x: x[1], reverse=True)
    
    cache = {}
    all_bitmasks = (1 << len(flows)) - 1
    max_pressure = 0
    
    for i in range(all_bitmasks + 1):
        pressure = dfs('AA', 26, i, distances, flows, cache) + dfs('AA', 26, all_bitmasks ^ i, distances, flows, cache)
        max_pressure = max(max_pressure, pressure)
    
    return max_pressure

def main(input_data):
    valves = parse_input(input_data)
    part1 = solve_part1(valves)
    part2 = solve_part2(valves)
    return part1, part2

# Example usage:
input_data = """
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""

part1, part2 = main(input_data)
print(f"Part 1: {part1}")
print(f"Part 2: {part2}")
