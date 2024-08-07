import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

let X = 1;
let cycle = 0;
const signalStrengths: number[] = [];
const interestingCycles = new Set([20, 60, 100, 140, 180, 220]);

const processInstruction = (instruction: string) => {
    const [command, value] = instruction.split(' ');
    if (command === 'noop') {
        cycle++;
        recordSignalStrength();
    } else if (command === 'addx') {
        cycle++;
        recordSignalStrength();
        cycle++;
        recordSignalStrength();
        X += Number(value);
    }
};

const recordSignalStrength = () => {
    if (interestingCycles.has(cycle)) {
        signalStrengths.push(cycle * X);
    }
};

input.forEach(processInstruction);
const totalSignalStrength = signalStrengths.reduce((sum, strength) => sum + strength, 0);
console.log(totalSignalStrength);