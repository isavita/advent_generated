import * as fs from 'fs';

const parseInput = (input: string): number[] => {
    return input.trim().split(',').map(Number);
};

const getParameter = (mode: number, value: number, memory: number[]): number => {
    return mode === 1 ? value : memory[value];
};

const runIntcodeProgram = (memory: number[], input: number): number[] => {
    let output: number[] = [];
    let pointer = 0;

    while (true) {
        const instruction = memory[pointer];
        const opcode = instruction % 100;
        const modes = [
            Math.floor(instruction / 100) % 10,
            Math.floor(instruction / 1000) % 10,
            Math.floor(instruction / 10000) % 10
        ];

        switch (opcode) {
            case 1: {
                const param1 = getParameter(modes[0], memory[pointer + 1], memory);
                const param2 = getParameter(modes[1], memory[pointer + 2], memory);
                const outputPosition = memory[pointer + 3];
                memory[outputPosition] = param1 + param2;
                pointer += 4;
                break;
            }
            case 2: {
                const param1 = getParameter(modes[0], memory[pointer + 1], memory);
                const param2 = getParameter(modes[1], memory[pointer + 2], memory);
                const outputPosition = memory[pointer + 3];
                memory[outputPosition] = param1 * param2;
                pointer += 4;
                break;
            }
            case 3: {
                const inputPosition = memory[pointer + 1];
                memory[inputPosition] = input;
                pointer += 2;
                break;
            }
            case 4: {
                const outputValue = getParameter(modes[0], memory[pointer + 1], memory);
                output.push(outputValue);
                pointer += 2;
                break;
            }
            case 99:
                return output;
            default:
                throw new Error(`Unknown opcode: ${opcode}`);
        }
    }
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const memory = parseInput(input);
    const output = runIntcodeProgram(memory, 1);
    console.log(output.pop());
};

main();