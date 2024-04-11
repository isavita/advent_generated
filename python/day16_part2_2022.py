from collections import defaultdict

def max_pressure(valves, curr, minute, pressure, open):
    max_pressure_value = pressure
    for next_valve in open:
        new_open = [v for v in open if v != next_valve]
        time_left = minute - valves[curr]['tunnels'][next_valve] - 1
        if time_left > 0:
            max_pressure_value = max(max_pressure_value, max_pressure(valves, next_valve, time_left, time_left * valves[next_valve]['flow'] + pressure, new_open))
    return max_pressure_value

def divide(l):
    if l == 1:
        return [
            [[], [0]],
            [[0], []]
        ]
    d = divide(l - 1)
    r = [[d[i][0] + [l - 1], d[i][1]] for i in range(len(d))] + [[d[i][0], d[i][1] + [l - 1]] for i in range(len(d))]
    return r

with open('input.txt', 'r') as file:
    input_data = file.read().strip()

valves = {}
for line in input_data.split('\n'):
    sp = line.split('; ')
    v = {'id': sp[0].split()[1], 'flow': int(sp[0].split('=')[1]), 'tunnels': {}}
    tunnels = sp[1].replace('tunnel leads to valve', '').replace('tunnels lead to valves', '').strip().split(', ')
    v['tunnels'] = {t: 1 for t in tunnels}
    v['tunnels'][v['id']] = 0
    valves[v['id']] = v

for k in valves:
    for i in valves:
        for j in valves:
            if k in valves[i]['tunnels'] and j in valves[k]['tunnels']:
                if j not in valves[i]['tunnels'] or valves[i]['tunnels'][j] > valves[i]['tunnels'][k] + valves[k]['tunnels'][j]:
                    valves[i]['tunnels'][j] = valves[i]['tunnels'][k] + valves[k]['tunnels'][j]

open_valves = [v['id'] for v in valves.values() if v['flow'] > 0]

max_total_pressure_value = 0
for d in divide(len(open_valves)):
    if not d[0] or not d[1]:
        continue
    mine = [open_valves[i] for i in d[0]]
    elephant = [open_valves[i] for i in d[1]]
    total_pressure = max_pressure(valves, 'AA', 26, 0, mine) + max_pressure(valves, 'AA', 26, 0, elephant)
    max_total_pressure_value = max(max_total_pressure_value, total_pressure)

print(max_total_pressure_value)