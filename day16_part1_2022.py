
import sys

def read_input(filename):
    with open(filename, 'r') as file:
        return file.read()

def max_pressure(valves, curr, minute, pressure, open):
    max_val = pressure
    for next_valve in open:
        new_open = [v for v in open if v != next_valve]
        time_left = minute - valves[curr]['tunnels'][next_valve] - 1
        if time_left > 0:
            max_val = max(max_val, max_pressure(valves, next_valve, time_left, time_left*valves[next_valve]['flow']+pressure, new_open))
    return max_val

def divide(l):
    if l == 1:
        return [([], [0]), ([0], [])]
    d = divide(l - 1)
    r = []
    for i in range(len(d)):
        r.append(([l - 1] + d[i][0], d[i][1]))
        r.append((d[i][0], [l - 1] + d[i][1]))
    return r

def main():
    valves = {}

    input_data = read_input("input.txt")
    for line in input_data.split("\n"):
        sp = line.split("; ")
        v = {}
        v['id'] = sp[0].split(' ')[1]
        v['flow'] = int(sp[0].split('=')[1])
        sp[1] = sp[1][len("tunnel leads to valve"):]
        if sp[1].startswith("s"):
            sp[1] = sp[1][2:]
        else:
            sp[1] = sp[1][1:]
        v['tunnels'] = {v['id']: 0}
        for t in sp[1].split(", "):
            v['tunnels'][t] = 1
        valves[v['id']] = v

    for k in valves:
        for i in valves:
            for j in valves:
                dik, okik = valves[i]['tunnels'].get(k, 0), k in valves[i]['tunnels']
                dkj, okkj = valves[k]['tunnels'].get(j, 0), k in valves[j]['tunnels']
                if okik and okkj:
                    dij, okij = valves[i]['tunnels'].get(j, 0), j in valves[i]['tunnels']
                    if not okij or dij > dik + dkj:
                        valves[i]['tunnels'][j] = dik + dkj

    open_valves = [v for v in valves if valves[v]['flow'] > 0]

    print(max_pressure(valves, "AA", 30, 0, open_valves))

if __name__ == "__main__":
    main()
