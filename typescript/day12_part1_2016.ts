import * as fs from 'fs';

interface Instruction {
    operation: string;
    args: string[];
}

function parseInput(input: string): Instruction[] {
    return input.split('\n').map(line => {
        const parts = line.split(' ');
        return {
            operation: parts[0],
            args: parts.slice(1)
        };
    });
}

function executeProgram(instructions: Instruction[]): number {
    const registers: { [key: string]: number } = { a: 0, b: 0, c: 0, d: 0 };
    let ip = 0; // Instruction pointer

    while (ip >= 0 && ip < instructions.length) {
        const { operation, args } = instructions[ip];

        switch (operation) {
            case 'cpy':
                const value = parseInt(args[0]) || registers[args[0]];
                registers[args[1]] = value;
                ip++;
                break;
            case 'inc':
                registers[args[0]]++;
                ip++;
                break;
            case 'dec':
                registers[args[0]]--;
                ip++;
                break;
            case 'jnz':
                const testValue = parseInt(args[0]) || registers[args[0]];
                if (testValue !== 0) {
                    ip += parseInt(args[1]);
                } else {
                    ip++;
                }
                break;
            default:
                throw new Error(`Unknown operation: ${operation}`);
        }
    }

    return registers['a'];
}

const input = fs.readFileSync('input.txt', 'utf-8');
const instructions = parseInput(input);
const result = executeProgram(instructions);

console.log(result);