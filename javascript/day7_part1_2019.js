const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split(',').map(Number);

const permutations = (arr) => {
    if (arr.length === 0) return [[]];

    const result = [];
    for (let i = 0; i < arr.length; i++) {
        const rest = [...arr.slice(0, i), ...arr.slice(i + 1)];
        const perms = permutations(rest);
        perms.forEach(p => result.push([arr[i], ...p]));
    }

    return result;
};

const runAmplifiers = (program, phaseSettings) => {
    const amplifiers = phaseSettings.map(phase => ({ program: [...program], inputs: [phase] }));
    let output = 0;

    while (!amplifiers[amplifiers.length - 1].halted) {
        amplifiers.forEach((amp, i) => {
            const { program, inputs } = amp;
            const { value, done } = runProgram(program, inputs, output);
            output = value;
            amp.program = program;
            amp.inputs = [];
            amp.halted = done;

            if (i === amplifiers.length - 1 && !done) {
                amplifiers[0].inputs.push(value);
            }
        });
    }

    return output;
};

const runProgram = (program, inputs, input) => {
    let i = 0;
    let output;
    while (i < program.length) {
        const opcode = program[i] % 100;
        const modes = Math.floor(program[i] / 100).toString().split('').reverse().map(Number);

        const getArgValue = (arg, mode) => {
            return mode === 1 ? arg : program[arg];
        };

        if (opcode === 99) {
            return { value: output, done: true };
        } else if (opcode === 1 || opcode === 2) {
            const arg1 = program[i + 1];
            const arg2 = program[i + 2];
            const arg3 = program[i + 3];
            const val1 = getArgValue(arg1, modes[0]);
            const val2 = getArgValue(arg2, modes[1]);

            program[arg3] = opcode === 1 ? val1 + val2 : val1 * val2;
            i += 4;
        } else if (opcode === 3) {
            program[program[i + 1]] = inputs.length > 0 ? inputs.shift() : input;
            i += 2;
        } else if (opcode === 4) {
            output = getArgValue(program[i + 1], modes[0]);
            i += 2;
        } else if (opcode === 5 || opcode === 6) {
            const arg1 = program[i + 1];
            const arg2 = program[i + 2];
            const val1 = getArgValue(arg1, modes[0]);
            const val2 = getArgValue(arg2, modes[1]);

            if ((opcode === 5 && val1 !== 0) || (opcode === 6 && val1 === 0)) {
                i = val2;
            } else {
                i += 3;
            }
        } else if (opcode === 7 || opcode === 8) {
            const arg1 = program[i + 1];
            const arg2 = program[i + 2];
            const arg3 = program[i + 3];
            const val1 = getArgValue(arg1, modes[0]);
            const val2 = getArgValue(arg2, modes[1]);

            program[arg3] = (opcode === 7 && val1 < val2) || (opcode === 8 && val1 === val2) ? 1 : 0;
            i += 4;
        } else {
            throw new Error(`Invalid opcode: ${opcode}`);
        }
    }
};

const phaseSettings = permutations([0, 1, 2, 3, 4]);
let maxOutput = 0;

phaseSettings.forEach(settings => {
    const output = runAmplifiers(input, settings);
    maxOutput = Math.max(maxOutput, output);
});

console.log(maxOutput);