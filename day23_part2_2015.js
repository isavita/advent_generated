const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const instructions = data.trim().split('\n');

let registers = { a: 1, b: 0 };

for (let i = 0; i < instructions.length; i++) {
    const parts = instructions[i].split(' ');

    switch (parts[0]) {
        case 'hlf':
            registers[parts[1]] /= 2;
            break;
        case 'tpl':
            registers[parts[1]] *= 3;
            break;
        case 'inc':
            registers[parts[1]]++;
            break;
        case 'jmp':
            i += parseInt(parts[1]) - 1;
            break;
        case 'jie':
            if (registers[parts[1][0]] % 2 === 0) {
                i += parseInt(parts[2]) - 1;
            }
            break;
        case 'jio':
            if (registers[parts[1][0]] === 1) {
                i += parseInt(parts[2]) - 1;
            }
            break;
        default:
            console.error(`Unknown instruction: ${parts[0]}`);
            process.exit(1);
    }
}

console.log(registers['b']);