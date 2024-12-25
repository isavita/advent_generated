
import * as fs from 'fs';

function readInput(filename: string): number[] {
    const fileContent = fs.readFileSync(filename, 'utf-8');
    return fileContent.trim().split(',').map(Number);
}

function runIntcode(program: number[], inputProvider: () => number, outputHandler: (value: number) => void): void {
    let memory = [...program];
    let instructionPointer = 0;
    let relativeBase = 0;

    const getValue = (mode: number, param: number): number => {
        switch (mode) {
            case 0: return memory[param] || 0;
            case 1: return param;
            case 2: return memory[relativeBase + param] || 0;
            default: throw new Error(`Invalid mode: ${mode}`);
        }
    };

    const setValue = (mode: number, param: number, value: number): void => {
        switch (mode) {
            case 0: memory[param] = value; break;
            case 2: memory[relativeBase + param] = value; break;
            default: throw new Error(`Invalid mode for set: ${mode}`);
        }
    };

    while (true) {
        const instruction = memory[instructionPointer];
        const opcode = instruction % 100;
        const mode1 = Math.floor(instruction / 100) % 10;
        const mode2 = Math.floor(instruction / 1000) % 10;
        const mode3 = Math.floor(instruction / 10000) % 10;

        if (opcode === 99) {
            break;
        }

        switch (opcode) {
            case 1: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const param3 = memory[instructionPointer + 3];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                setValue(mode3, param3, value1 + value2);
                instructionPointer += 4;
                break;
            }
            case 2: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const param3 = memory[instructionPointer + 3];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                setValue(mode3, param3, value1 * value2);
                instructionPointer += 4;
                break;
            }
            case 3: {
                const param1 = memory[instructionPointer + 1];
                const inputValue = inputProvider();
                setValue(mode1, param1, inputValue);
                instructionPointer += 2;
                break;
            }
            case 4: {
                const param1 = memory[instructionPointer + 1];
                const outputValue = getValue(mode1, param1);
                outputHandler(outputValue);
                instructionPointer += 2;
                break;
            }
            case 5: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                if (value1 !== 0) {
                    instructionPointer = value2;
                } else {
                    instructionPointer += 3;
                }
                break;
            }
            case 6: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                if (value1 === 0) {
                    instructionPointer = value2;
                } else {
                    instructionPointer += 3;
                }
                break;
            }
            case 7: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const param3 = memory[instructionPointer + 3];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                setValue(mode3, param3, value1 < value2 ? 1 : 0);
                instructionPointer += 4;
                break;
            }
            case 8: {
                const param1 = memory[instructionPointer + 1];
                const param2 = memory[instructionPointer + 2];
                const param3 = memory[instructionPointer + 3];
                const value1 = getValue(mode1, param1);
                const value2 = getValue(mode2, param2);
                setValue(mode3, param3, value1 === value2 ? 1 : 0);
                instructionPointer += 4;
                break;
            }
            case 9: {
                const param1 = memory[instructionPointer + 1];
                const value1 = getValue(mode1, param1);
                relativeBase += value1;
                instructionPointer += 2;
                break;
            }
            default:
                throw new Error(`Unknown opcode: ${opcode}`);
        }
    }
}

function solvePart1(program: number[]): number {
    let blockCount = 0;
    let outputCounter = 0;
    let x = 0;
    let y = 0;

    const outputHandler = (value: number) => {
        if (outputCounter % 3 === 0) {
            x = value;
        } else if (outputCounter % 3 === 1) {
            y = value;
        } else {
            if (value === 2) {
                blockCount++;
            }
        }
        outputCounter++;
    };

    runIntcode(program, () => 0, outputHandler);
    return blockCount;
}

function solvePart2(program: number[]): number {
    program[0] = 2;
    let score = 0;
    let paddleX = 0;
    let ballX = 0;
    let outputCounter = 0;
    let x = 0;
    let y = 0;

    const inputProvider = () => {
        if (ballX < paddleX) return -1;
        if (ballX > paddleX) return 1;
        return 0;
    };

    const outputHandler = (value: number) => {
        if (outputCounter % 3 === 0) {
            x = value;
        } else if (outputCounter % 3 === 1) {
            y = value;
        } else {
            if (x === -1 && y === 0) {
                score = value;
            } else if (value === 3) {
                paddleX = x;
            } else if (value === 4) {
                ballX = x;
            }
        }
        outputCounter++;
    };

    runIntcode(program, inputProvider, outputHandler);
    return score;
}

const program = readInput('input.txt');
console.log('Part 1:', solvePart1(program));
console.log('Part 2:', solvePart2(program));
