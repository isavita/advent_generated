const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const [accumulator] = executeBootCode(input);
console.log(accumulator);

function executeBootCode(instructions) {
    let accumulator = 0;
    const visited = new Set();
    let currentInstruction = 0;

    while (currentInstruction < instructions.length) {
        if (visited.has(currentInstruction)) {
            return [accumulator, true];
        }

        visited.add(currentInstruction);
        const [op, arg] = instructions[currentInstruction].split(' ');
        
        switch (op) {
            case 'acc':
                accumulator += parseInt(arg);
                currentInstruction++;
                break;
            case 'jmp':
                currentInstruction += parseInt(arg);
                break;
            case 'nop':
                currentInstruction++;
                break;
        }
    }

    return [accumulator, false];
}