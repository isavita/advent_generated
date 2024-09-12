const fs = require('fs');

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

function executeProgram(ipRegister, instructions, initialR0) {
    const registers = [initialR0, 0, 0, 0, 0, 0];
    let ip = 0;

    while (ip >= 0 && ip < instructions.length) {
        registers[ipRegister] = ip;
        const [opcode, a, b, c] = instructions[ip];
        opcodes[opcode](registers, a, b, c);
        ip = registers[ipRegister];
        ip++;
    }

    return registers[0];
}

function solve() {
    const input = fs.readFileSync('input.txt', 'utf8').split('\n');
    const ipRegister = parseInt(input[0].split(' ')[1]);
    const instructions = input.slice(1).map(line => {
        const [opcode, ...params] = line.split(' ');
        return [opcode, ...params.map(Number)];
    });

    const part1 = executeProgram(ipRegister, instructions, 0);
    const part2 = executeProgram(ipRegister, instructions, 1);

    console.log(`Part 1: ${part1}`);
    console.log(`Part 2: ${part2}`);
}

solve();
