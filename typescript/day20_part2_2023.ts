import * as fs from 'fs';

type Module = FlipFlop | Conjunction | Broadcaster;

interface FlipFlop {
  name: string;
  moduleType: number;
  connectsTo: string[];
  state: boolean;
}

interface Conjunction {
  name: string;
  moduleType: number;
  connectsTo: string[];
  watches: Map<string, boolean>;
}

interface Broadcaster {
  name: string;
  moduleType: number;
  connectsTo: string[];
}

const BROADCASTER = 0;
const FLIP_FLOP = 1;
const CONJUNCTION = 2;

function handleLine(line: string, connections: Map<string, Module>) {
  if (line.includes("broadcaster")) {
    const module: Broadcaster = {
      moduleType: BROADCASTER,
      name: line.split(" -> ")[0],
      connectsTo: line.split(" -> ")[1].split(", "),
    };
    connections.set(module.name, module);
  } else if (line.includes("%")) {
    const module: FlipFlop = {
      moduleType: FLIP_FLOP,
      name: line.split(" -> ")[0].slice(1),
      connectsTo: line.split(" -> ")[1].split(", "),
      state: false,
    };
    connections.set(module.name, module);
  } else {
    const module: Conjunction = {
      moduleType: CONJUNCTION,
      name: line.split(" -> ")[0].slice(1),
      connectsTo: line.split(" -> ")[1].split(", "),
      watches: new Map(),
    };
    connections.set(module.name, module);
  }
}

function completeWatches(connections: Map<string, Module>) {
  for (const [name, module] of connections) {
    if (module.moduleType === CONJUNCTION) {
      const conj = module as Conjunction;
      for (const [name2, module2] of connections) {
        if (module2.moduleType === FLIP_FLOP || module2.moduleType === CONJUNCTION) {
          if (module2.connectsTo.includes(conj.name)) {
            conj.watches.set(module2.name, false);
          }
        }
      }
      connections.set(conj.name, conj);
    }
  }
}

interface State {
  from: string;
  name: string;
  pulse: boolean;
}

function simulatePress(connections: Map<string, Module>, loops: Map<string, number>, pressNumber: number): [number[], boolean] {
  const queue: State[] = [{ from: "button", name: "broadcaster", pulse: false }];
  const pulses: number[] = [1, 0];

  let found = false;
  while (queue.length > 0) {
    const currState = queue.shift()!;
    const module = connections.get(currState.name);

    if (!module) continue; // Skip if module is undefined

    if (currState.name === "out") continue;
    if (currState.name === "rx" && !currState.pulse) found = true;

    const pulse = currState.pulse;

    if (module.moduleType === BROADCASTER) {
      const broadcaster = module as Broadcaster;
      for (const name of broadcaster.connectsTo) {
        queue.push({ from: broadcaster.name, name, pulse });
        pulses[pulse ? 1 : 0]++;
      }
    } else if (module.moduleType === FLIP_FLOP) {
      const flipFlop = module as FlipFlop;
      if (!pulse) {
        flipFlop.state = !flipFlop.state;
        for (const name of flipFlop.connectsTo) {
          queue.push({ from: flipFlop.name, name, pulse: flipFlop.state });
          pulses[flipFlop.state ? 1 : 0]++;
        }
      }
    } else if (module.moduleType === CONJUNCTION) {
      const conjunction = module as Conjunction;
      conjunction.watches.set(currState.from, pulse);
      const allTrue = Array.from(conjunction.watches.values()).every(Boolean);
      for (const name of conjunction.connectsTo) {
        queue.push({ from: conjunction.name, name, pulse: !allTrue });
        pulses[!allTrue ? 1 : 0]++;
      }
      if (loops.has(currState.name) && !allTrue && loops.get(currState.name) === -1) {
        loops.set(currState.name, pressNumber);
      }
    }
  }
  return [pulses, found];
}

function main() {
  const data = fs.readFileSync('input.txt', 'utf-8').split('\n');
  const connections = new Map<string, Module>();

  for (const line of data) {
    handleLine(line, connections);
  }

  completeWatches(connections);

  const pxPrev: string[] = [];
  for (const [name, module] of connections) {
    if (module.connectsTo.includes("rx")) {
      pxPrev.push(name);
    }
  }

  if (pxPrev.length !== 1) throw new Error("Error: more than one pxPrev");

  const conj = connections.get(pxPrev[0]) as Conjunction;
  const loopLengths = new Map<string, number>();
  for (const name of conj.watches.keys()) {
    loopLengths.set(name, -1);
  }

  let pressNumber = 0;
  while (true) {
    pressNumber++;
    const [_, found] = simulatePress(connections, loopLengths, pressNumber);
    if (found) break;
    if (Array.from(loopLengths.values()).every(length => length !== -1)) break;
  }

  let sum = 1;
  for (const length of loopLengths.values()) {
    sum *= length;
  }

  console.log(sum);
}

main();