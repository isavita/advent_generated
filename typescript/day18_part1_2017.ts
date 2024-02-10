
const fs = require('fs');

function getValue(arg, registers) {
    if (!isNaN(parseInt(arg))) {
        return parseInt(arg);
    }
    return registers.get(arg) || 0;
}

const instructions = fs.readFileSync('input.txt', 'utf8')
    .trim()
    .split('\n')
    .map(line => line.trim().split(' '));

const registers = new Map();
let lastSound = 0;

for (let i = 0; i < instructions.length;) {
    const instruction = instructions[i];
    const cmd = instruction[0];
    const arg1 = instruction[1];

    switch (cmd) {
        case "snd":
            lastSound = getValue(arg1, registers);
            break;
        case "set":
            registers.set(arg1, getValue(instruction[2], registers));
            break;
        case "add":
            registers.set(arg1, registers.get(arg1) + getValue(instruction[2], registers));
            break;
        case "mul":
            registers.set(arg1, registers.get(arg1) * getValue(instruction[2], registers));
            break;
        case "mod":
            registers.set(arg1, registers.get(arg1) % getValue(instruction[2], registers));
            break;
        case "rcv":
            if (getValue(arg1, registers) !== 0) {
                console.log(lastSound);
                process.exit(0);
            }
            break;
        case "jgz":
            if (getValue(arg1, registers) > 0) {
                i += getValue(instruction[2], registers);
                continue;
            }
            break;
    }
    i++;
}
