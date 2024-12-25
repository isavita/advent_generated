
import * as fs from 'fs';

type Instruction = {
    type: string;
    args: string[];
};

function parseInput(input: string): Instruction[] {
    return input.trim().split('\n').map(line => {
        const parts = line.split(' ');
        return {
            type: parts[0],
            args: parts.slice(1)
        };
    });
}

function runProgram(instructions: Instruction[], initialA: number): number {
    const registers: { [key: string]: number } = { a: initialA, b: 0, c: 0, d: 0 };
    let instructionPointer = 0;

    while (instructionPointer >= 0 && instructionPointer < instructions.length) {
        const instruction = instructions[instructionPointer];

        const getValue = (arg: string): number => {
            if (isNaN(parseInt(arg))) {
                return registers[arg] || 0;
            } else {
                return parseInt(arg);
            }
        };

        switch (instruction.type) {
            case 'cpy':
                const value = getValue(instruction.args[0]);
                if (!isNaN(parseInt(instruction.args[1]))) {
                    instructionPointer++;
                    break;
                }
                registers[instruction.args[1]] = value;
                instructionPointer++;
                break;
            case 'inc':
                registers[instruction.args[0]]++;
                instructionPointer++;
                break;
            case 'dec':
                registers[instruction.args[0]]--;
                instructionPointer++;
                break;
            case 'jnz':
                if (getValue(instruction.args[0]) !== 0) {
                    instructionPointer += getValue(instruction.args[1]);
                } else {
                    instructionPointer++;
                }
                break;
            case 'tgl':
                const toggleIndex = instructionPointer + getValue(instruction.args[0]);
                if (toggleIndex >= 0 && toggleIndex < instructions.length) {
                    const targetInstruction = instructions[toggleIndex];
                    if (targetInstruction.args.length === 1) {
                        if (targetInstruction.type === 'inc') {
                            targetInstruction.type = 'dec';
                        } else {
                            targetInstruction.type = 'inc';
                        }
                    } else if (targetInstruction.args.length === 2) {
                        if (targetInstruction.type === 'jnz') {
                            targetInstruction.type = 'cpy';
                        } else {
                            targetInstruction.type = 'jnz';
                        }
                    }
                }
                instructionPointer++;
                break;
            default:
                instructionPointer++;
        }
    }

    return registers.a;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const instructions = parseInput(input);

    // Part 1
    const resultPart1 = runProgram([...instructions.map(i => ({...i}))], 7);
    console.log("Part 1:", resultPart1);

    // Part 2
    const resultPart2 = runProgram([...instructions.map(i => ({...i}))], 12);
    console.log("Part 2:", resultPart2);
}

main();
