const fs = require('fs');

// Read input file
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

// Parse input
const ipRegister = parseInt(input[0].split(' ')[1]);
const instructions = input.slice(1).map(line => {
    const [op, ...args] = line.split(' ');
    return [op, ...args.map(Number)];
});

// Define opcodes
const opcodes = {
    addr: (r, a, b, c) => { r[c] = r[a] + r[b]; },
    addi: (r, a, b, c) => { r[c] = r[a] + b; },
    mulr: (r, a, b, c) => { r[c] = r[a] * r[b]; },
    muli: (r, a, b, c) => { r[c] = r[a] * b; },
    banr: (r, a, b, c) => { r[c] = r[a] & r[b]; },
    bani: (r, a, b, c) => { r[c] = r[a] & b; },
    borr: (r, a, b, c) => { r[c] = r[a] | r[b]; },
    bori: (r, a, b, c) => { r[c] = r[a] | b; },
    setr: (r, a, b, c) => { r[c] = r[a]; },
    seti: (r, a, b, c) => { r[c] = a; },
    gtir: (r, a, b, c) => { r[c] = a > r[b] ? 1 : 0; },
    gtri: (r, a, b, c) => { r[c] = r[a] > b ? 1 : 0; },
    gtrr: (r, a, b, c) => { r[c] = r[a] > r[b] ? 1 : 0; },
    eqir: (r, a, b, c) => { r[c] = a === r[b] ? 1 : 0; },
    eqri: (r, a, b, c) => { r[c] = r[a] === b ? 1 : 0; },
    eqrr: (r, a, b, c) => { r[c] = r[a] === r[b] ? 1 : 0; },
};

// Execute program
function executeProgram() {
    const registers = [0, 0, 0, 0, 0, 0];
    let ip = 0;

    while (ip >= 0 && ip < instructions.length) {
        registers[ipRegister] = ip;
        const [op, a, b, c] = instructions[ip];
        opcodes[op](registers, a, b, c);
        ip = registers[ipRegister];
        ip++;
    }

    return registers[0];
}

// Print result
console.log(executeProgram());
