function solve(input) {
    const lines = input.trim().split('\n');
    const ipRegister = parseInt(lines[0].split(' ')[1]);
    const instructions = lines.slice(1).map(line => {
        const [op, ...params] = line.split(' ');
        return [op, ...params.map(Number)];
    });

    function execute(registers, [op, a, b, c]) {
        switch (op) {
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
    }

    function analyzeProgram() {
        const registers = [0, 0, 0, 0, 0, 0];
        let ip = 0;
        const seen = new Set();
        let lastR1 = 0;

        while (ip >= 0 && ip < instructions.length) {
            registers[ipRegister] = ip;
            execute(registers, instructions[ip]);
            ip = registers[ipRegister];
            ip++;

            // Look for patterns in register 1
            if (ip === 28) {
                const key = registers[1];
                if (seen.has(key)) {
                    return lastR1;
                }
                seen.add(key);
                lastR1 = key;
            }
        }
    }

    return analyzeProgram();
}
