import * as fs from 'fs';

type PulseValue = 0 | 1;

const Low: PulseValue = 0;
const High: PulseValue = 1;

const FlipFlop: string = '%';
const Conjunction: string = '&';

interface Module {
    name: string;
    prefix: string;
    destinations: string[];
    state: boolean;
    memory: Record<string, PulseValue>;
}

interface Pulse {
    value: PulseValue;
    fromName: string;
    toName: string;
}

function parseInput(input: string[]): Record<string, Module> {
    const prefixes = [FlipFlop, Conjunction];
    const modules: Record<string, Module> = {};

    for (const line of input) {
        const parts = line.split(' -> ');
        let module: Module = { name: '', prefix: '', destinations: [], state: false, memory: {} };
        let isPrefix = false;

        for (const prefix of prefixes) {
            if (parts[0].startsWith(prefix)) {
                module.prefix = prefix;
                module.name = parts[0].slice(1);
                isPrefix = true;
                break;
            }
        }

        if (!isPrefix) {
            module.name = parts[0];
        }

        module.destinations = parts[1].split(', ');
        module.memory = {};
        modules[module.name] = module;
    }

    for (const module of Object.values(modules)) {
        for (const destName of module.destinations) {
            if (modules[destName] && modules[destName].prefix === Conjunction) {
                modules[destName].memory[module.name] = Low;
            }
        }
    }

    return modules;
}

function pushButton(modules: Record<string, Module>, startPulse: Pulse, numCycle: number): [number, number] {
    let cntLow = 0;
    let cntHigh = 0;
    const pulseQueue: Pulse[] = [];

    for (let i = 0; i < numCycle; i++) {
        pulseQueue.push(startPulse);

        while (pulseQueue.length > 0) {
            const pulse = pulseQueue.shift()!;

            if (pulse.value === Low) {
                cntLow++;
            } else {
                cntHigh++;
            }

            if (!(pulse.toName in modules)) {
                continue;
            }

            const module = modules[pulse.toName];
            let newPulseValue: PulseValue;

            switch (module.prefix) {
                case FlipFlop:
                    if (pulse.value === Low) {
                        module.state = !module.state;
                        newPulseValue = module.state ? High : Low;
                    } else {
                        continue;
                    }
                    break;

                case Conjunction:
                    module.memory[pulse.fromName] = pulse.value;
                    const isHighForAll = Object.values(module.memory).every(value => value === High);
                    newPulseValue = isHighForAll ? Low : High;
                    break;

                default:
                    newPulseValue = pulse.value;
            }

            for (const destName of module.destinations) {
                const newPulse: Pulse = {
                    value: newPulseValue,
                    fromName: pulse.toName,
                    toName: destName,
                };
                pulseQueue.push(newPulse);
            }
        }
    }

    return [cntLow, cntHigh];
}

function solve(input: string[]): number {
    const startPulse: Pulse = {
        value: Low,
        fromName: 'button',
        toName: 'broadcaster',
    };
    const numCycle = 1000;
    const modules = parseInput(input);
    const [cntLow, cntHigh] = pushButton(modules, startPulse, numCycle);
    return cntLow * cntHigh;
}

function readFile(fileName: string): string[] {
    const data = fs.readFileSync(fileName, 'utf-8');
    return data.trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));