const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const ipBind = parseInt(input[0].split(' ')[1]);
const instructions = input.slice(1).map(line => line.split(' '));

const registers = Array(6).fill(0);

for (let ip = 0; ip < instructions.length; ip++) {
    registers[ipBind] = ip;
    const [opcode, a, b, c] = instructions[ip].map((x, i) => i === 0 ? x : parseInt(x));

    switch (opcode) {
        case "addr":
            registers[c] = registers[a] + registers[b];
            break;
        case "addi":
            registers[c] = registers[a] + b;
            break;
        case "mulr":
            registers[c] = registers[a] * registers[b];
            break;
        case "muli":
            registers[c] = registers[a] * b;
            break;
        case "banr":
            registers[c] = registers[a] & registers[b];
            break;
        case "bani":
            registers[c] = registers[a] & b;
            break;
        case "borr":
            registers[c] = registers[a] | registers[b];
            break;
        case "bori":
            registers[c] = registers[a] | b;
            break;
        case "setr":
            registers[c] = registers[a];
            break;
        case "seti":
            registers[c] = a;
            break;
        case "gtir":
            registers[c] = a > registers[b] ? 1 : 0;
            break;
        case "gtri":
            registers[c] = registers[a] > b ? 1 : 0;
            break;
        case "gtrr":
            registers[c] = registers[a] > registers[b] ? 1 : 0;
            break;
        case "eqir":
            registers[c] = a === registers[b] ? 1 : 0;
            break;
        case "eqri":
            registers[c] = registers[a] === b ? 1 : 0;
            break;
        case "eqrr":
            registers[c] = registers[a] === registers[b] ? 1 : 0;
            break;
    }

    ip = registers[ipBind];
    if (ip < 0 || ip >= instructions.length) {
        break;
    }
}

console.log(registers[0]);