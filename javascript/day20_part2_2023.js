const fs = require('fs');

function check(e) {
	if (e !== null) {
		throw e;
	}
}

class FlipFlop {
	constructor(name, moduleType, connectsTo, state) {
		this.name = name;
		this.moduleType = moduleType;
		this.connectsTo = connectsTo;
		this.state = state;
	}
}

class Conjunction {
	constructor(name, moduleType, connectsTo, watches) {
		this.name = name;
		this.moduleType = moduleType;
		this.connectsTo = connectsTo;
		this.watches = watches;
	}
}

class Broadcaster {
	constructor(name, moduleType, connectsTo) {
		this.name = name;
		this.moduleType = moduleType;
		this.connectsTo = connectsTo;
	}
}

const BROADCASTER = 0;
const FLIP_FLOP = 1;
const CONJUNCTION = 2;

function handleLine(line, connections) {
	if (line.includes("broadcaster")) {
		const module = new Broadcaster();
		module.moduleType = BROADCASTER;
		const str = line.split(" -> ");
		module.name = str[0];
		module.connectsTo = str[1].split(", ");
		connections[module.name] = module;
	} else if (line.includes("%")) {
		const module = new FlipFlop();
		module.moduleType = FLIP_FLOP;
		const str = line.split(" -> ");
		module.name = str[0].slice(1);
		module.connectsTo = str[1].split(", ");
		module.state = false;
		connections[module.name] = module;
	} else {
		const module = new Conjunction();
		module.moduleType = CONJUNCTION;
		const str = line.split(" -> ");
		module.name = str[0].slice(1);
		module.connectsTo = str[1].split(", ");
		module.watches = {};
		connections[module.name] = module;
	}
}

function completeWatches(connections) {
	for (const module of Object.values(connections)) {
		if (module instanceof Conjunction) {
			const conj = module;

			for (const module2 of Object.values(connections)) {
				if (module2 instanceof FlipFlop || module2 instanceof Conjunction) {
					for (const name of module2.connectsTo) {
						if (name === conj.name) {
							conj.watches[module2.name] = false;
						}
					}
				}
			}
			connections[conj.name] = conj;
		}
	}
}

class State {
	constructor(from, name, pulse) {
		this.from = from;
		this.name = name;
		this.pulse = pulse;
	}
}

function simulatePress(connections, loops, pressNumber) {
	let queue = [];

	const pulses = [1, 0];

	const originalConnections = Object.assign({}, connections);

	queue.push(new State("button", "broadcaster", false));

	let steps = 0;
	let found = false;
	while (queue.length > 0) {
		const currState = queue.shift();

		const module = connections[currState.name];
		if (currState.name === "out") {
			continue;
		}

		if (currState.name === "rx" && !currState.pulse) {
			found = true;
		}

		const pulse = currState.pulse;

		if (module instanceof Broadcaster) {
			for (const name of module.connectsTo) {
				queue.push(new State(module.name, name, pulse));
				if (pulse) {
					pulses[1]++;
				} else {
					pulses[0]++;
				}
			}
			connections[currState.name] = module;
		} else if (module instanceof FlipFlop) {
			if (!pulse) {
				module.state = !module.state;
				for (const name of module.connectsTo) {
					connections[currState.name] = module;
					queue.push(new State(module.name, name, module.state));
					if (module.state) {
						pulses[1]++;
					} else {
						pulses[0]++;
					}
				}
			}
			connections[currState.name] = module;
		} else if (module instanceof Conjunction) {
			module.watches[currState.from] = pulse;
			connections[currState.name] = module;

			let allTrue = true;
			for (const state of Object.values(module.watches)) {
				if (!state) {
					allTrue = false;
					break;
				}
			}

			for (const name of module.connectsTo) {
				queue.push(new State(module.name, name, !allTrue));
				if (!allTrue) {
					pulses[1]++;
				} else {
					pulses[0]++;
				}
			}
			connections[currState.name] = module;

			const currLoop = loops[currState.name];
			if (currLoop !== undefined && !allTrue && currLoop === -1) {
				loops[currState.name] = pressNumber;
			}
		}

		steps++;
	}
	return [pulses, found];
}

function sumHistory(hist) {
	const sum = [0, 0];
	for (const pulses of hist) {
		sum[0] += pulses[0];
		sum[1] += pulses[1];
	}
	return sum[0] * sum[1];
}

function connectsTo(from, to, connections) {
	const module = connections[from];
	if (module instanceof Broadcaster) {
		for (const name of module.connectsTo) {
			if (name === to) {
				return true;
			}
		}
	} else if (module instanceof FlipFlop) {
		for (const name of module.connectsTo) {
			if (name === to) {
				return true;
			}
		}
	} else if (module instanceof Conjunction) {
		for (const name of module.connectsTo) {
			if (name === to) {
				return true;
			}
		}
	}
	return false;
}

function copyConnections(connections) {
	const copy = {};
	for (const [k, v] of Object.entries(connections)) {
		copy[k] = v;
	}
	return copy;
}

const input = fs.readFileSync("input.txt", "utf8");
const lines = input.split("\n");

const connections = {};

for (const line of lines) {
	handleLine(line, connections);
}

completeWatches(connections);

const pxPrev = [];
for (const k in connections) {
	if (connectsTo(k, "rx", connections)) {
		pxPrev.push(k);
	}
}

if (pxPrev.length !== 1) {
	throw new Error("Error: more than one pxPrev");
}

let conj;
if (connections[pxPrev[0]] instanceof Conjunction) {
	conj = connections[pxPrev[0]];
} else {
	throw new Error("Error: pxPrev is not a conjunction");
}

const loopLengths = {};
for (const name of Object.keys(conj.watches)) {
	loopLengths[name] = -1;
}

let pressNumber = 0;
while (true) {
	pressNumber++;
	const [_, found] = simulatePress(connections, loopLengths, pressNumber);
	if (found) {
		break;
	}
	let complete = true;
	for (const length of Object.values(loopLengths)) {
		if (length === -1) {
			complete = false;
			break;
		}
	}
	if (complete) {
		break;
	}
}

let sum = 1;
for (const length of Object.values(loopLengths)) {
	sum *= length;
}

console.log(sum);