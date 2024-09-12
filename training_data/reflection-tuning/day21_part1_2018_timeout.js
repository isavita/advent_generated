const fs = require('fs');

// Read and parse input
const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const ipRegister = parseInt(input.shift().split(' ')[1]);
const instructions = input.map(line => {
    const [op, ...args] = line.split(' ');
    return [op, ...args.map(Number)];
});

// Define operations
const ops = {
    addr: (r, a, b) => r[a] + r[b],
    addi: (r, a, b) => r[a] + b,
    mulr: (r, a, b) => r[a] * r[b],
    muli: (r, a, b) => r[a] * b,
    banr: (r, a, b) => r[a] & r[b],
    bani: (r, a, b) => r[a] & b,
    borr: (r, a, b) => r[a] | r[b],
    bori: (r, a, b) => r[a] | b,
    setr: (r, a) => r[a],
    seti: (r, a) => a,
    gtir: (r, a, b) => a > r[b] ? 1 : 0,
    gtri: (r, a, b) => r[a] > b ? 1 : 0,
    gtrr: (r, a, b) => r[a] > r[b] ? 1 : 0,
    eqir: (r, a, b) => a === r[b] ? 1 : 0,
    eqri: (r, a, b) => r[a] === b ? 1 : 0,
    eqrr: (r, a, b) => r[a] === r[b] ? 1 : 0,
};

function runProgram(initialR0, maxInstructions = 1000000) {
    const registers = [initialR0, 0, 0, 0, 0, 0];
    let ip = 0;
    let instructionsExecuted = 0;

    while (ip >= 0 && ip < instructions.length && instructionsExecuted < maxInstructions) {
        registers[ipRegister] = ip;
        const [op, a, b, c] = instructions[ip];
        registers[c] = ops[op](registers, a, b);
        ip = registers[ipRegister];
        ip++;
        instructionsExecuted++;
    }

    return instructionsExecuted < maxInstructions ? instructionsExecuted : Infinity;
}

function findLowestHaltingR0() {
    let r0 = 0;
    let minInstructions = Infinity;
    let result = -1;

    while (true) {
        const instructions = runProgram(r0);
        if (instructions < minInstructions) {
            minInstructions = instructions;
            result = r0;
            console.log(`New minimum found: R0 = ${r0}, Instructions = ${minInstructions}`);
            if (minInstructions < Infinity) {
                break;
            }
        }
        r0++;
    }

    return result;
}

console.log("Lowest non-negative integer for register 0:", findLowestHaltingR0());
