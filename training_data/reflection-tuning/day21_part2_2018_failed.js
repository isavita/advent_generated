function executeProgram(instructions, registers, ip) {
    let steps = 0;
    while (registers[ip] >= 0 && registers[ip] < instructions.length) {
        const [opcode, a, b, c] = instructions[registers[ip]];
        switch (opcode) {
            case 'addr': registers[c] = registers[a] + registers[b]; break;
            case 'addi': registers[c] = registers[a] + b; break;
            case 'mulr': registers[c] = registers[a] * registers[b]; break;
            case 'muli': registers[c] = registers[a] * b; break;
            case 'banr': registers[c] = registers[a] & registers[b]; break;
            case 'bani': registers[c] = registers[a] & b; break;
            case 'borr': registers[c] = registers[a] | registers[b]; break;
            case 'bori': registers[c] = registers[a] | b; break;
            case 'setr': registers[c] = registers[a]; break;
            case 'seti': registers[c] = a; break;
            case 'gtir': registers[c] = a > registers[b] ? 1 : 0; break;
            case 'gtri': registers[c] = registers[a] > b ? 1 : 0; break;
            case 'gtrr': registers[c] = registers[a] > registers[b] ? 1 : 0; break;
            case 'eqir': registers[c] = a === registers[b] ? 1 : 0; break;
            case 'eqri': registers[c] = registers[a] === b ? 1 : 0; break;
            case 'eqrr': registers[c] = registers[a] === registers[b] ? 1 : 0; break;
        }
        registers[ip]++;
        steps++;
    }
    return steps;
}

function solve(input) {
    const lines = input.trim().split('\n');
    const ip = parseInt(lines[0].split(' ')[1]);
    const instructions = lines.slice(1).map(line => {
        const [opcode, ...params] = line.split(' ');
        return [opcode, ...params.map(Number)];
    });

    // Part 1
    let minSteps = Infinity;
    let part1Result = 0;
    for (let i = 0; i < 1000000; i++) {
        const steps = executeProgram([...instructions], [i, 0, 0, 0, 0, 0], ip);
        if (steps < minSteps) {
            minSteps = steps;
            part1Result = i;
        }
    }

    // Part 2
    let maxSteps = 0;
    let part2Result = 0;
    for (let i = 0; i < 1000000; i++) {
        const steps = executeProgram([...instructions], [i, 0, 0, 0, 0, 0], ip);
        if (steps > maxSteps) {
            maxSteps = steps;
            part2Result = i;
        }
    }

    return `Part 1: ${part1Result}\nPart 2: ${part2Result}`;
}

// Example usage:
// const input = `#ip 0
// seti 5 0 1
// seti 6 0 2
// addi 0 1 0
// addr 1 2 3
// setr 1 0 0
// seti 8 0 4
// seti 9 0 5`;
// console.log(solve(input));
