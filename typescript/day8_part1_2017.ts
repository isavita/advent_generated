import * as fs from 'fs';
import * as path from 'path';

interface Instruction {
    register: string;
    operation: string;
    value: number;
    conditionRegister: string;
    conditionOperator: string;
    conditionValue: number;
}

function parseInstructions(input: string): Instruction[] {
    return input.split('\n')
        .filter(line => line.trim() !== '')
        .map(line => {
            const [register, operation, value, _, conditionRegister, conditionOperator, conditionValue] = line.split(' ');
            return {
                register,
                operation,
                value: parseInt(value, 10),
                conditionRegister,
                conditionOperator,
                conditionValue: parseInt(conditionValue, 10)
            };
        });
}

function evaluateCondition(registers: Record<string, number>, condition: Instruction): boolean {
    const registerValue = registers[condition.conditionRegister] || 0;
    switch (condition.conditionOperator) {
        case '>': return registerValue > condition.conditionValue;
        case '<': return registerValue < condition.conditionValue;
        case '>=': return registerValue >= condition.conditionValue;
        case '<=': return registerValue <= condition.conditionValue;
        case '==': return registerValue === condition.conditionValue;
        case '!=': return registerValue !== condition.conditionValue;
        default: throw new Error(`Unknown operator: ${condition.conditionOperator}`);
    }
}

function processInstructions(instructions: Instruction[]): number {
    const registers: Record<string, number> = {};

    for (const instruction of instructions) {
        if (evaluateCondition(registers, instruction)) {
            const registerValue = registers[instruction.register] || 0;
            const newValue = instruction.operation === 'inc'
                ? registerValue + instruction.value
                : registerValue - instruction.value;
            registers[instruction.register] = newValue;
        }
    }

    return Math.max(...Object.values(registers));
}

function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(filePath, 'utf8');
    const instructions = parseInstructions(input);
    const largestValue = processInstructions(instructions);
    console.log(largestValue);
}

main();