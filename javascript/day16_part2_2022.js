const fs = require('fs');

class Valve {
  constructor(id, flow) {
    this.id = id;
    this.flow = flow;
    this.tunnels = { [id]: 0 };
  }
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
const valves = {};

input.split('\n').forEach(line => {
  const [, id, flow, tunnels] = line.match(/Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? (.*)/);
  const v = new Valve(id, parseInt(flow));
  tunnels.split(', ').forEach(t => v.tunnels[t] = 1);
  valves[id] = v;
});

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

let max = 0;
for (const d of divide(open.length)) {
  if (d[0].length === 0 || d[1].length === 0) continue;
  const mine = d[0].map(i => open[i]);
  const elephant = d[1].map(i => open[i]);
  const x = maxPressure(valves, 'AA', 26, 0, mine) + maxPressure(valves, 'AA', 26, 0, elephant);
  max = Math.max(max, x);
}

console.log(max);

function maxPressure(valves, curr, minute, pressure, open) {
  let max = pressure;
  for (const next of open) {
    const newopen = open.filter(v => v !== next);
    const timeLeft = minute - valves[curr].tunnels[next] - 1;
    if (timeLeft > 0) {
      max = Math.max(max, maxPressure(valves, next, timeLeft, timeLeft * valves[next].flow + pressure, newopen));
    }
  }
  return max;
}

function* divide(l) {
  if (l === 1) {
    yield [[], [0]];
    yield [[0], []];
  } else {
    for (const d of divide(l - 1)) {
      yield [[l - 1, ...d[0]], d[1]];
      yield [d[0], [l - 1, ...d[1]]];
    }
  }
}