const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let instructions = input.map(instruction => {
    const [op, arg] = instruction.split(' ');
    return { op, arg: parseInt(arg) };
});

for (let i = 0; i < instructions.length; i++) {
    let { op, arg } = instructions[i];
    if (op === 'acc') {
        continue;
    }

    let modifiedInstructions = [...instructions];
    if (op === 'jmp') {
        modifiedInstructions[i] = { op: 'nop', arg };
    } else {
        modifiedInstructions[i] = { op: 'jmp', arg };
    }

    let [accumulator, terminated] = executeBootCode(modifiedInstructions);
    if (terminated) {
        console.log(accumulator);
        break;
    }
}

function executeBootCode(instructions) {
    let accumulator = 0;
    let visited = new Set();
    let currentInstruction = 0;

    while (currentInstruction < instructions.length) {
        if (visited.has(currentInstruction)) {
            return [accumulator, false];
        }

        visited.add(currentInstruction);
        let { op, arg } = instructions[currentInstruction];

        switch (op) {
            case 'acc':
                accumulator += arg;
                currentInstruction++;
                break;
            case 'jmp':
                currentInstruction += arg;
                break;
            case 'nop':
                currentInstruction++;
                break;
        }
    }

    return [accumulator, true];
}