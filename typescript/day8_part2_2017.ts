import * as fs from 'fs';

interface Instruction {
    register: string;
    operation: string;
    value: number;
    conditionRegister: string;
    conditionOperator: string;
    conditionValue: number;
}

function parseInstruction(line: string): Instruction {
    const parts = line.split(' ');
    return {
        register: parts[0],
        operation: parts[1],
        value: parseInt(parts[2], 10),
        conditionRegister: parts[4],
        conditionOperator: parts[5],
        conditionValue: parseInt(parts[6], 10)
    };
}

function evaluateCondition(registers: Record<string, number>, conditionRegister: string, condition: string, value: number): boolean {
    switch (condition) {
        case '>': return registers[conditionRegister] > value;
        case '<': return registers[conditionRegister] < value;
        case '>=': return registers[conditionRegister] >= value;
        case '<=': return registers[conditionRegister] <= value;
        case '==': return registers[conditionRegister] === value;
        case '!=': return registers[conditionRegister] !== value;
        default: throw new Error(`Unknown condition: ${condition}`);
    }
}

function processInstructions(instructions: Instruction[]): { maxValue: number, highestValueEver: number } {
    const registers: Record<string, number> = {};
    let maxValue = Number.MIN_SAFE_INTEGER;
    let highestValueEver = Number.MIN_SAFE_INTEGER;

    for (const instruction of instructions) {
        if (!registers[instruction.register]) {
            registers[instruction.register] = 0;
        }
        if (!registers[instruction.conditionRegister]) {
            registers[instruction.conditionRegister] = 0;
        }

        if (evaluateCondition(registers, instruction.conditionRegister, instruction.conditionOperator, instruction.conditionValue)) {
            if (instruction.operation === 'inc') {
                registers[instruction.register] += instruction.value;
            } else if (instruction.operation === 'dec') {
                registers[instruction.register] -= instruction.value;
            }

            highestValueEver = Math.max(highestValueEver, registers[instruction.register]);
        }

        maxValue = Math.max(maxValue, registers[instruction.register]);
    }

    return { maxValue, highestValueEver };
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const instructions = input.split('\n').map(parseInstruction);
    const result = processInstructions(instructions);

    console.log(`The largest value in any register is: ${result.maxValue}`);
    console.log(`The highest value ever held in any register is: ${result.highestValueEver}`);
}

main();