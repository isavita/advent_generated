import * as fs from 'fs';

type OP = {
    name: string;
    action: (a: number, b: number) => number;
    a: 'r' | 'v';
    b: 'r' | 'v';
    matchCount: number[];
};

const opcodes: OP[] = [
    { name: "addr", action: (a, b) => a + b, a: 'r', b: 'r', matchCount: [] },
    { name: "addi", action: (a, b) => a + b, a: 'r', b: 'v', matchCount: [] },
    { name: "mulr", action: (a, b) => a * b, a: 'r', b: 'r', matchCount: [] },
    { name: "muli", action: (a, b) => a * b, a: 'r', b: 'v', matchCount: [] },
    { name: "banr", action: (a, b) => a & b, a: 'r', b: 'r', matchCount: [] },
    { name: "bani", action: (a, b) => a & b, a: 'r', b: 'v', matchCount: [] },
    { name: "borr", action: (a, b) => a | b, a: 'r', b: 'r', matchCount: [] },
    { name: "bori", action: (a, b) => a | b, a: 'r', b: 'v', matchCount: [] },
    { name: "setr", action: (a) => a, a: 'r', b: 'r', matchCount: [] },
    { name: "seti", action: (a) => a, a: 'v', b: 'r', matchCount: [] },
    { name: "gtir", action: (a, b) => (a > b ? 1 : 0), a: 'v', b: 'r', matchCount: [] },
    { name: "gtri", action: (a, b) => (a > b ? 1 : 0), a: 'r', b: 'v', matchCount: [] },
    { name: "gtrr", action: (a, b) => (a > b ? 1 : 0), a: 'r', b: 'r', matchCount: [] },
    { name: "eqir", action: (a, b) => (a === b ? 1 : 0), a: 'v', b: 'r', matchCount: [] },
    { name: "eqri", action: (a, b) => (a === b ? 1 : 0), a: 'r', b: 'v', matchCount: [] },
    { name: "eqrr", action: (a, b) => (a === b ? 1 : 0), a: 'r', b: 'r', matchCount: [] },
];

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
let sum = 0;

for (let i = 0; i < input.length; i += 4) {
    if (input[i][0] === 'B') {
        const registers = input[i].match(/\d+/g)!.map(Number);
        const instruction = input[i + 1].match(/\d+/g)!.map(Number);
        const n = input[i + 2].match(/\d+/g)!.map(Number);
        const tempSum = testCode(registers, n, instruction);

        if (tempSum >= 3) sum++;
    } else break;
}

console.log(sum);

function testCode(registers: number[], n: number[], instruction: number[]): number {
    return opcodes.reduce((count, op) => {
        const result = runOp(op, registers, instruction);
        if (match(n, result)) {
            op.matchCount.push(instruction[0]);
            return count + 1;
        }
        return count;
    }, 0);
}

function match(r: number[], c: number[]): boolean {
    return r.length === c.length && r.every((val, index) => val === c[index]);
}

function runOp(op: OP, registers: number[], instruction: number[]): number[] {
    const regCopy = [...registers];
    const A = op.a === 'r' ? regCopy[instruction[1]] : instruction[1];
    const B = op.b === 'r' ? regCopy[instruction[2]] : instruction[2];
    regCopy[instruction[3]] = op.action(A, B);
    return regCopy;
}