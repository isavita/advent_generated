
import * as fs from 'fs';

interface Program {
    a: bigint;
    b: bigint;
    c: bigint;
    program: number[];
}

function computeOperand(val: number, a: bigint, b: bigint, c: bigint): bigint {
    switch (val) {
        case 0:
        case 1:
        case 2:
        case 3:
            return BigInt(val);
        case 4:
            return a;
        case 5:
            return b;
        case 6:
            return c;
        default:
            throw new Error(`Invalid combo operand: ${val}`);
    }
}

function simulateComputer(program: Program): number[] {
    const outs: number[] = [];
    let { a, b, c } = program;
    const input = program.program;

    for (let i = 1; i <= input.length; i += 2) {
        const cmd = input[i - 1];
        switch (cmd) {
            case 0:
                a >>= computeOperand(input[i], a, b, c);
                break;
            case 1:
                b ^= BigInt(input[i]);
                break;
            case 2:
                b = computeOperand(input[i], a, b, c) % 8n;
                break;
            case 3:
                if (a !== 0n) {
                    i = input[i] - 1;
                }
                break;
            case 4:
                b ^= c;
                break;
            case 5:
                outs.push(Number(computeOperand(input[i], a, b, c) % 8n));
                break;
            case 6:
                b = a >> computeOperand(input[i], a, b, c);
                break;
            case 7:
                c = a >> computeOperand(input[i], a, b, c);
                break;
            default:
                throw new Error(`Invalid opcode: ${cmd}`);
        }
    }
    return outs;
}

interface Pair {
    a: number;
    b: bigint;
}

function check(p: Program): bigint[] {
    const program = p.program;
    const valids: bigint[] = [];
    const stack: Pair[] = [{ a: 0, b: 0n }];
    const seen = new Map<string, boolean>();

    while (stack.length > 0) {
        const state = stack.pop()!;
        const key = `${state.a},${state.b}`;

        if (seen.has(key)) {
            continue;
        }
        seen.set(key, true);

        const depth = state.a;
        const score = state.b;

        if (depth === program.length) {
            valids.push(score);
        } else {
            for (let i = 0n; i < 8n; i++) {
                const newScore = i + 8n * score;
                const testProgram: Program = { a: newScore, b: p.b, c: p.c, program };
                const result = simulateComputer(testProgram);
                if (result.length > 0 && result[0] === program[program.length - 1 - depth]) {
                    stack.push({ a: depth + 1, b: newScore });
                }
            }
        }
    }
    return valids;
}

function main() {
    const fileContent = fs.readFileSync('input.txt', 'utf-8');
    let a: bigint = 0n;
    let b: bigint = 0n;
    let c: bigint = 0n;
    let program: number[] = [];

    const lines = fileContent.trim().split('\n');
    for (const line of lines) {
        const trimmedLine = line.trim();
        if (trimmedLine.startsWith('Register A:')) {
            a = BigInt(parseInt(trimmedLine.split(':')[1].trim()));
        } else if (trimmedLine.startsWith('Register B:')) {
            b = BigInt(parseInt(trimmedLine.split(':')[1].trim()));
        } else if (trimmedLine.startsWith('Register C:')) {
            c = BigInt(parseInt(trimmedLine.split(':')[1].trim()));
        } else if (trimmedLine.startsWith('Program:')) {
            program = trimmedLine.split(':')[1].trim().split(',').map(Number);
        }
    }

    const p: Program = { a, b, c, program };
    const validValues = check(p);
    let minVal = validValues[0];
    for (const val of validValues) {
        if (val < minVal) {
            minVal = val;
        }
    }

    console.log(minVal.toString());
}

main();
