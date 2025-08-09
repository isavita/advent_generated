
#!/usr/bin/env bash
python3 - <<'PY'
import sys, re, itertools

INF = 10**9
TIME_LIMIT = 26

valve_id = {}
flow = {}
neighbors = {}
with open('input.txt') as f:
    for line in f:
        m = re.match(r'Valve (\w{2}) has flow rate=(\d+);', line)
        if not m: continue
        v, f = m.group(1), int(m.group(2))
        valve_id[v] = v
        flow[v] = f
        nbrs = re.findall(r'([A-Z]{2})', line.split('to ')[1])
        neighbors[v] = nbrs

# all-pairs shortest paths
dist = {i: {j: (0 if i == j else INF) for j in valve_id} for i in valve_id}
for v, nbrs in neighbors.items():
    for n in nbrs:
        dist[v][n] = 1

for k in valve_id:
    for i in valve_id:
        for j in valve_id:
            if dist[i][j] > dist[i][k] + dist[k][j]:
                dist[i][j] = dist[i][k] + dist[k][j]

open_valves = [v for v, f in flow.items() if f > 0]
idx = {v: i for i, v in enumerate(open_valves)}
num_open = len(open_valves)
total_masks = 1 << num_open

memo = {}
def solve(cur, time_left, mask):
    if time_left <= 0: return 0
    key = (cur, time_left, mask)
    if key in memo: return memo[key]
    best = 0
    for i, v in enumerate(open_valves):
        if mask & (1 << i):
            travel = dist[cur][v]
            t = time_left - travel - 1
            if t > 0:
                pressure = flow[v] * t
                best = max(best, pressure + solve(v, t, mask ^ (1 << i)))
    memo[key] = best
    return best

start = 'AA'
ans = 0
for my_mask in range(total_masks):
    elephant_mask = (total_masks - 1) ^ my_mask
    ans = max(ans, solve(start, TIME_LIMIT, my_mask) + solve(start, TIME_LIMIT, elephant_mask))

print(ans)
PY
