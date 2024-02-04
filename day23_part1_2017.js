const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map(line => line.trim());

let mulCount = 0;
let pointer = 0;
const registers = {};
const instructions = input;

const getValue = (s) => {
    if (!isNaN(s)) {
        return parseInt(s);
    }
    return registers[s] || 0;
};

while (pointer >= 0 && pointer < instructions.length) {
    const parts = instructions[pointer].split(' ');
    const cmd = parts[0];
    const x = parts[1];
    const y = parts[2];

    switch (cmd) {
        case 'set':
            registers[x] = getValue(y);
            break;
        case 'sub':
            registers[x] = (registers[x] || 0) - getValue(y);
            break;
        case 'mul':
            registers[x] = (registers[x] || 0) * getValue(y);
            mulCount++;
            break;
        case 'jnz':
            if (getValue(x) !== 0) {
                pointer += getValue(y) - 1;
            }
            break;
    }
    pointer++;
}

console.log(mulCount);