
const fs = require('fs');

const instructions = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let a = 1;

while (true) {
    if (producesClockSignal(a, instructions)) {
        console.log(a);
        break;
    }
    a++;
}

function producesClockSignal(a, instructions) {
    const registers = { a, b: 0, c: 0, d: 0 };
    let lastOutput = 0;
    let outputCount = 0;

    for (let i = 0; i < instructions.length;) {
        const parts = instructions[i].split(' ');
        switch (parts[0]) {
            case 'cpy':
                const copyVal = getValue(parts[1], registers);
                registers[parts[2]] = copyVal;
                break;
            case 'inc':
                registers[parts[1]]++;
                break;
            case 'dec':
                registers[parts[1]]--;
                break;
            case 'jnz':
                const jumpVal = getValue(parts[1], registers);
                if (jumpVal !== 0) {
                    const jump = parseInt(parts[2]);
                    i += jump;
                    continue;
                }
                break;
            case 'out':
                const outputVal = getValue(parts[1], registers);
                if (outputVal !== 0 && outputVal !== 1) {
                    return false;
                }
                if (outputCount > 0 && outputVal === lastOutput) {
                    return false;
                }
                lastOutput = outputVal;
                outputCount++;
                if (outputCount > 50) {
                    return true;
                }
                break;
        }
        i++;
    }
    return false;
}

function getValue(s, registers) {
    const val = parseInt(s);
    if (isNaN(val)) {
        return registers[s];
    }
    return val;
}
