const fs = require('fs');

class Valve {
  constructor(id, flow, tunnels) {
    this.id = id;
    this.flow = flow;
    this.tunnels = tunnels;
  }
}

function readAll(path) {
  return fs.readFileSync(path, 'utf8').trim();
}

function maxPressure(valves, curr, minute, pressure, open, d) {
  let max = pressure;
  for (const next of open) {
    const newOpen = open.filter(v => v !== next);
    const timeLeft = minute - valves[curr].tunnels[next] - 1;
    if (timeLeft > 0) {
      max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newOpen, d + 1));
    }
  }
  return max;
}

function main() {
  const input = readAll('input.txt');
  const valves = {};
  for (const line of input.split('\n')) {
    const [, id, flow, tunnels] = line.match(/Valve (\w+) has flow rate=(\d+); tunnel[s]? lead[s]? to valve[s]? (.+)/);
    valves[id] = new Valve(id, parseInt(flow), Object.fromEntries(tunnels.split(', ').map(t => [t, 1])));
  }

  for (const k of Object.keys(valves)) {
    for (const i of Object.keys(valves)) {
      for (const j of Object.keys(valves)) {
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

  const open = Object.values(valves)
    .filter(v => v.flow > 0)
    .map(v => v.id);

  console.log(maxPressure(valves, 'AA', 30, 0, open, 0));
}

main();