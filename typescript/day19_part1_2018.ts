import * as fs from 'fs';

type Instruction = (a: number, b: number, c: number, registers: number[]) => void;

const operations: Record<string, Instruction> = {
    addr: (a, b, c, r) => r[c] = r[a] + r[b],
    addi: (a, b, c, r) => r[c] = r[a] + b,
    mulr: (a, b, c, r) => r[c] = r[a] * r[b],
    muli: (a, b, c, r) => r[c] = r[a] * b,
    banr: (a, b, c, r) => r[c] = r[a] & r[b],
    bani: (a, b, c, r) => r[c] = r[a] & b,
    borr: (a, b, c, r) => r[c] = r[a] | r[b],
    bori: (a, b, c, r) => r[c] = r[a] | b,
    setr: (a, _, c, r) => r[c] = r[a],
    seti: (a, _, c, r) => r[c] = a,
    gtir: (a, b, c, r) => r[c] = a > r[b] ? 1 : 0,
    gtri: (a, b, c, r) => r[c] = r[a] > b ? 1 : 0,
    gtrr: (a, b, c, r) => r[c] = r[a] > r[b] ? 1 : 0,
    eqir: (a, b, c, r) => r[c] = a === r[b] ? 1 : 0,
    eqri: (a, b, c, r) => r[c] = r[a] === b ? 1 : 0,
    eqrr: (a, b, c, r) => r[c] = r[a] === r[b] ? 1 : 0,
};

const runProgram = (instructions: string[], ipBinding: number) => {
    const registers = Array(6).fill(0);
    let ip = 0;

    while (ip >= 0 && ip < instructions.length) {
        registers[ipBinding] = ip;
        const [opcode, aStr, bStr, cStr] = instructions[ip].split(' ');
        const a = parseInt(aStr);
        const b = parseInt(bStr);
        const c = parseInt(cStr);
        operations[opcode](a, b, c, registers);
        ip = registers[ipBinding] + 1;
    }

    return registers[0];
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const ipBinding = parseInt(input[0].split(' ')[1]);
    const instructions = input.slice(1);
    const result = runProgram(instructions, ipBinding);
    console.log(result);
};

main();