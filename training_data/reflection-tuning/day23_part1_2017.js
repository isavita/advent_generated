const fs = require('fs');

function solve(input) {
    const instructions = input.trim().split('\n').map(line => line.split(' '));
    const registers = { a: 0, b: 0, c: 0, d: 0, e: 0, f: 0, g: 0, h: 0 };
    let mulCount = 0;
    let pc = 0;

    function getValue(x) {
        return isNaN(x) ? registers[x] : parseInt(x);
    }

    while (pc >= 0 && pc < instructions.length) {
        const [cmd, x, y] = instructions[pc];
        switch (cmd) {
            case 'set':
                registers[x] = getValue(y);
                break;
            case 'sub':
                registers[x] -= getValue(y);
                break;
            case 'mul':
                registers[x] *= getValue(y);
                mulCount++;
                break;
            case 'jnz':
                if (getValue(x) !== 0) {
                    pc += getValue(y) - 1;
                }
                break;
        }
        pc++;
    }

    return mulCount;
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solve(input));