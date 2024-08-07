import * as fs from 'fs';

const runIntcode = (program: number[], input: number): number => {
    let pointer = 0;
    const output: number[] = [];

    const getParameter = (mode: number, value: number): number => 
        mode === 0 ? program[value] : value;

    while (true) {
        const instruction = program[pointer];
        const opcode = instruction % 100;
        const modes = [
            Math.floor(instruction / 100) % 10,
            Math.floor(instruction / 1000) % 10,
            Math.floor(instruction / 10000) % 10,
        ];

        switch (opcode) {
            case 1: {
                const [a, b, c] = program.slice(pointer + 1, pointer + 4);
                program[c] = getParameter(modes[0], a) + getParameter(modes[1], b);
                pointer += 4;
                break;
            }
            case 2: {
                const [a, b, c] = program.slice(pointer + 1, pointer + 4);
                program[c] = getParameter(modes[0], a) * getParameter(modes[1], b);
                pointer += 4;
                break;
            }
            case 3: {
                const address = program[pointer + 1];
                program[address] = input;
                pointer += 2;
                break;
            }
            case 4: {
                const a = program[pointer + 1];
                output.push(getParameter(modes[0], a));
                pointer += 2;
                break;
            }
            case 5: {
                const [a, b] = program.slice(pointer + 1, pointer + 3);
                pointer = getParameter(modes[0], a) !== 0 ? getParameter(modes[1], b) : pointer + 3;
                break;
            }
            case 6: {
                const [a, b] = program.slice(pointer + 1, pointer + 3);
                pointer = getParameter(modes[0], a) === 0 ? getParameter(modes[1], b) : pointer + 3;
                break;
            }
            case 7: {
                const [a, b, c] = program.slice(pointer + 1, pointer + 4);
                program[c] = getParameter(modes[0], a) < getParameter(modes[1], b) ? 1 : 0;
                pointer += 4;
                break;
            }
            case 8: {
                const [a, b, c] = program.slice(pointer + 1, pointer + 4);
                program[c] = getParameter(modes[0], a) === getParameter(modes[1], b) ? 1 : 0;
                pointer += 4;
                break;
            }
            case 99:
                return output[output.length - 1];
            default:
                throw new Error(`Unknown opcode: ${opcode}`);
        }
    }
};

const main = () => {
    const inputData = fs.readFileSync('input.txt', 'utf-8').trim();
    const program = inputData.split(',').map(Number);
    const diagnosticCode = runIntcode(program, 5);
    console.log(diagnosticCode);
};

main();