
import * as fs from 'fs';

interface Valve {
    id: string;
    flow: number;
    tunnels: Map<string, number>;
}

function readAll(path: string): string {
    try {
        return fs.readFileSync(path, 'utf-8').trim();
    } catch (e) {
        throw e;
    }
}

function parseInput(input: string): Map<string, Valve> {
    const valves = new Map<string, Valve>();
    for (const line of input.split('\n')) {
        const parts = line.split('; ');
        const valveMatch = parts[0].match(/Valve (\w+) has flow rate=(\d+)/);
        if (!valveMatch) continue;
        const id = valveMatch[1];
        const flow = parseInt(valveMatch[2], 10);
        let tunnelsStr = parts[1].substring(parts[1].indexOf('valve') + 5).trim();
        if (tunnelsStr.startsWith('s')) {
            tunnelsStr = tunnelsStr.substring(2);
        }
        const tunnels = new Map<string, number>();
        tunnels.set(id, 0);
        for (const tunnel of tunnelsStr.split(', ')) {
            tunnels.set(tunnel, 1);
        }
        valves.set(id, { id, flow, tunnels });
    }
    return valves;
}

function floydWarshall(valves: Map<string, Valve>): void {
    for (const k of valves.keys()) {
        for (const i of valves.keys()) {
            for (const j of valves.keys()) {
                const valveI = valves.get(i)!;
                const valveK = valves.get(k)!;
                const valveJ = valves.get(j)!;
                const dik = valveI.tunnels.get(k);
                const dkj = valveK.tunnels.get(j);
                if (dik !== undefined && dkj !== undefined) {
                    const dij = valveI.tunnels.get(j);
                    if (dij === undefined || dij > dik + dkj) {
                        valveI.tunnels.set(j, dik + dkj);
                    }
                }
            }
        }
    }
}

function getOpenValves(valves: Map<string, Valve>): string[] {
    const open: string[] = [];
    for (const v of valves.values()) {
        if (v.flow > 0) {
            open.push(v.id);
        }
    }
    return open;
}

function maxPressure(valves: Map<string, Valve>, curr: string, minute: number, pressure: number, open: string[]): number {
    let max = pressure;
    for (let i = 0; i < open.length; i++) {
        const next = open[i];
        const newOpen = open.slice(0, i).concat(open.slice(i + 1));
        const timeLeft = minute - valves.get(curr)!.tunnels.get(next)! - 1;
        if (timeLeft > 0) {
            max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves.get(next)!.flow + pressure, newOpen));
        }
    }
    return max;
}

function divide(l: number): [number[], number[]][] {
    if (l === 1) {
        return [
            [[], [0]],
            [[0], []],
        ];
    }
    const d = divide(l - 1);
    const r: [number[], number[]][] = [];
    for (const [mine, elephant] of d) {
        r.push([[l - 1, ...mine], elephant]);
        r.push([mine, [l - 1, ...elephant]]);
    }
    return r;
}

function solve(): void {
    const input = readAll('input.txt');
    const valves = parseInput(input);
    floydWarshall(valves);
    const open = getOpenValves(valves);

    let max = 0;
    for (const [mineIndices, elephantIndices] of divide(open.length)) {
        if (mineIndices.length === 0 || elephantIndices.length === 0) {
            continue;
        }
        const mine = mineIndices.map(i => open[i]);
        const elephant = elephantIndices.map(i => open[i]);
        const x = maxPressure(valves, 'AA', 26, 0, mine) + maxPressure(valves, 'AA', 26, 0, elephant);
        max = Math.max(max, x);
    }
    console.log(max);
}

solve();
