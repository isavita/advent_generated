import * as fs from 'fs';

const runIntcode = (program: number[]): number[] => {
    let pointer = 0;
    while (true) {
        const opcode = program[pointer];
        if (opcode === 99) break;

        const [param1, param2, outputPos] = program.slice(pointer + 1, pointer + 4);
        if (opcode === 1) {
            program[outputPos] = program[param1] + program[param2];
        } else if (opcode === 2) {
            program[outputPos] = program[param1] * program[param2];
        }
        pointer += 4;
    }
    return program;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const program = input.trim().split(',').map(Number);
    
    program[1] = 12;
    program[2] = 2;

    const result = runIntcode(program);
    console.log(result[0]);
};

main();