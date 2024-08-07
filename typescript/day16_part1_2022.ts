import * as fs from 'fs';

interface Valve {
    id: string;
    flow: number;
    tunnels: { [key: string]: number };
}

function readAll(path: string): string {
    return fs.readFileSync(path, 'utf-8').trim();
}

function maxPressure(valves: { [key: string]: Valve }, curr: string, minute: number, pressure: number, open: string[], d: number): number {
    let max = pressure;
    for (const next of open) {
        const newopen = open.filter(v => v !== next);
        const timeLeft = minute - valves[curr].tunnels[next] - 1;
        if (timeLeft > 0) {
            max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newopen, d + 1));
        }
    }
    return max;
}

function divide(l: number): [number[], number[]][] {
    if (l === 1) {
        return [[[], [0]], [[0], []]];
    }
    const d = divide(l - 1);
    const r: [number[], number[]][] = [];
    for (const [left, right] of d) {
        r.push([[l - 1, ...left], right]);
        r.push([left, [l - 1, ...right]]);
    }
    return r;
}

const input = readAll('input.txt');
const valves: { [key: string]: Valve } = {};

for (const line of input.split('\n')) {
    const sp = line.split('; ');
    const v: Valve = { id: '', flow: 0, tunnels: {} };
    const match = sp[0].match(/Valve (\w+) has flow rate=(\d+)/);
    if (match) {
        v.id = match[1];
        v.flow = parseInt(match[2]);
    }
    const tunnels = sp[1].replace('tunnel leads to valve ', '').replace('tunnels lead to valves ', '').split(', ');
    v.tunnels = { [v.id]: 0, ...Object.fromEntries(tunnels.map(t => [t, 1])) };
    valves[v.id] = v;
}

for (const k in valves) {
    for (const i in valves) {
        for (const j in valves) {
            const dik = valves[i].tunnels[k];
            const dkj = valves[k].tunnels[j];
            if (dik !== undefined && dkj !== undefined) {
                const dij = valves[i].tunnels[j];
                if (dij === undefined || dij > dik + dkj) {
                    valves[i].tunnels[j] = dik + dkj;
                }
            }
        }
    }
}

const open = Object.values(valves).filter(v => v.flow > 0).map(v => v.id);
console.log(maxPressure(valves, 'AA', 30, 0, open, 0));