from collections import defaultdict

def parse_input(lines):
    valves = {}
    tunnels = defaultdict(list)
    for line in lines:
        parts = line.split()
        valve = parts[1]
        flow_rate = int(parts[4].split('=')[1].rstrip(';'))
        connected = [v.rstrip(',') for v in parts[9:]]
        valves[valve] = flow_rate
        tunnels[valve] = connected
    return valves, tunnels

def dfs(valve, time, opened, valves, tunnels, cache):
    if time <= 0:
        return 0
    
    key = (valve, time, frozenset(opened))
    if key in cache:
        return cache[key]
    
    max_pressure = 0
    
    # Try opening the current valve
    if valve not in opened and valves[valve] > 0:
        new_opened = opened | {valve}
        pressure = (time - 1) * valves[valve] + dfs(valve, time - 1, new_opened, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    # Try moving to connected valves
    for next_valve in tunnels[valve]:
        pressure = dfs(next_valve, time - 1, opened, valves, tunnels, cache)
        max_pressure = max(max_pressure, pressure)
    
    cache[key] = max_pressure
    return max_pressure

def main():
    with open('input.txt', 'r') as file:
        lines = file.readlines()
    
    valves, tunnels = parse_input(lines)
    cache = {}
    max_pressure = dfs('AA', 30, set(), valves, tunnels, cache)
    print(f"The most pressure you can release is: {max_pressure}")

if __name__ == "__main__":
    main()
