const fs = require('fs');

class Module {
    constructor(name, prefix, destinations, state, memory) {
        this.name = name;
        this.prefix = prefix;
        this.destinations = destinations;
        this.state = state;
        this.memory = memory;
    }
}

class Pulse {
    constructor(value, fromName, toName) {
        this.value = value;
        this.fromName = fromName;
        this.toName = toName;
    }
}

const PulseValue = {
    Low: 0,
    High: 1
};

const FlipFlop = '%';
const Conjunction = '&';

function parseInput(input) {
    const prefixes = [FlipFlop, Conjunction];
    const modules = {};

    input.forEach(line => {
        const parts = line.split(" -> ");

        let module = new Module();
        let isPrefix = false;
        prefixes.forEach(prefix => {
            if (parts[0][0] === prefix) {
                module.prefix = prefix;
                module.name = parts[0].slice(1);
                isPrefix = true;
            }
        });
        if (!isPrefix) {
            module.name = parts[0];
        }
        module.destinations = parts[1].split(", ");
        module.memory = {};

        modules[module.name] = module;
    });

    for (const module of Object.values(modules)) {
        module.destinations.forEach(destName => {
            if (modules[destName] && modules[destName].prefix === Conjunction) {
                modules[destName].memory[module.name] = PulseValue.Low;
            }
        });
    }

    return modules;
}

function pushButton(modules, startPulse, numCycle) {
    let cntLow = 0;
    let cntHigh = 0;
    let pulseQueue = [];

    for (let i = 0; i < numCycle; i++) {
        pulseQueue.push(startPulse);

        while (pulseQueue.length > 0) {
            const pulse = pulseQueue.shift();

            if (pulse.value === PulseValue.Low) {
                cntLow++;
            } else {
                cntHigh++;
            }

            if (!modules[pulse.toName]) {
                continue;
            }

            const module = modules[pulse.toName];
            let newPulseValue;
            switch (module.prefix) {
                case FlipFlop:
                    if (pulse.value === PulseValue.Low) {
                        module.state = !module.state;
                        if (module.state) {
                            newPulseValue = PulseValue.High;
                        } else {
                            newPulseValue = PulseValue.Low;
                        }
                    } else {
                        continue;
                    }
                    break;

                case Conjunction:
                    module.memory[pulse.fromName] = pulse.value;
                    let isHighForAll = true;
                    Object.values(module.memory).forEach(value => {
                        if (value === PulseValue.Low) {
                            isHighForAll = false;
                        }
                    });

                    if (isHighForAll) {
                        newPulseValue = PulseValue.Low;
                    } else {
                        newPulseValue = PulseValue.High;
                    }
                    break;

                default:
                    newPulseValue = pulse.value;
            }

            module.destinations.forEach(destName => {
                const newPulse = new Pulse(newPulseValue, pulse.toName, destName);
                pulseQueue.push(newPulse);
            });
        }
    }

    return [cntLow, cntHigh];
}

function solve(input) {
    const startPulse = new Pulse(PulseValue.Low, "button", "broadcaster");
    const numCycle = 1000;

    const modules = parseInput(input);

    const [cntLow, cntHigh] = pushButton(modules, startPulse, numCycle);

    return cntLow * cntHigh;
}

const input = fs.readFileSync("input.txt", "utf8").trim().split("\n");
console.log(solve(input));