function executeProgram(instructions, initialReg0 = 0) {
    const registers = [initialReg0, 0, 0, 0, 0, 0];
    const ipRegister = parseInt(instructions[0].split(' ')[1]);
    let ip = 0;

    const opcodes = {
        addr: (a, b, c) => registers[c] = registers[a] + registers[b],
        addi: (a, b, c) => registers[c] = registers[a] + b,
        mulr: (a, b, c) => registers[c] = registers[a] * registers[b],
        muli: (a, b, c) => registers[c] = registers[a] * b,
        banr: (a, b, c) => registers[c] = registers[a] & registers[b],
        bani: (a, b, c) => registers[c] = registers[a] & b,
        borr: (a, b, c) => registers[c] = registers[a] | registers[b],
        bori: (a, b, c) => registers[c] = registers[a] | b,
        setr: (a, b, c) => registers[c] = registers[a],
        seti: (a, b, c) => registers[c] = a,
        gtir: (a, b, c) => registers[c] = a > registers[b] ? 1 : 0,
        gtri: (a, b, c) => registers[c] = registers[a] > b ? 1 : 0,
        gtrr: (a, b, c) => registers[c] = registers[a] > registers[b] ? 1 : 0,
        eqir: (a, b, c) => registers[c] = a === registers[b] ? 1 : 0,
        eqri: (a, b, c) => registers[c] = registers[a] === b ? 1 : 0,
        eqrr: (a, b, c) => registers[c] = registers[a] === registers[b] ? 1 : 0
    };

    while (ip >= 0 && ip < instructions.length - 1) {
        registers[ipRegister] = ip;
        const [opcode, a, b, c] = instructions[ip + 1].split(' ');
        opcodes[opcode](parseInt(a), parseInt(b), parseInt(c));
        ip = registers[ipRegister];
        ip++;
    }

    return registers[0];
}

function solve(input) {
    const instructions = input.split('\n');
    const part1 = executeProgram(instructions);
    const part2 = executeProgram(instructions, 1);
    return { part1, part2 };
}

// Example usage:
const input = `#ip 0
seti 5 0 1
seti 6 0 2
addi 0 1 0
addr 1 2 3
setr 1 0 0
seti 8 0 4
seti 9 0 5`;

const result = solve(input);
console.log(result);
