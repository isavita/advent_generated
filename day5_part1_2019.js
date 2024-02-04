const fs = require('fs');

function getMode(instruction, position) {
    return Math.floor(instruction / Math.pow(10, position + 1)) % 10;
}

function getParam(program, pointer, mode) {
    if (mode === 0) {
        return program[program[pointer]];
    }
    return program[pointer];
}

function runProgram(program, input) {
    let output = 0;
    for (let pointer = 0; pointer < program.length;) {
        const instruction = program[pointer];
        const opcode = instruction % 100;

        switch (opcode) {
            case 1:
            case 2:
                const param1 = getParam(program, pointer + 1, getMode(instruction, 1));
                const param2 = getParam(program, pointer + 2, getMode(instruction, 2));
                const result = opcode === 1 ? param1 + param2 : param1 * param2;
                program[program[pointer + 3]] = result;
                pointer += 4;
                break;
            case 3:
                program[program[pointer + 1]] = input;
                pointer += 2;
                break;
            case 4:
                output = getParam(program, pointer + 1, getMode(instruction, 1));
                pointer += 2;
                break;
            case 99:
                return output;
            default:
                throw new Error(`Unknown opcode: ${opcode}`);
        }
    }
    return output;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        throw err;
    }
    const strProgram = data.trim().split(',');
    const program = strProgram.map(s => parseInt(s));
    console.log(runProgram(program, 1));
});