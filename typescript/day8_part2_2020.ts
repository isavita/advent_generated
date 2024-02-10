const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let instructions = [];
for (let i = 0; i < input.length; i++) {
    instructions.push(input[i]);
}

for (let i = 0; i < instructions.length; i++) {
    let [op, arg] = parseInstruction(instructions[i]);
    if (op === "acc") {
        continue;
    }

    let modifiedInstructions = [...instructions];
    if (op === "jmp") {
        modifiedInstructions[i] = `nop ${arg}`;
    } else {
        modifiedInstructions[i] = `jmp ${arg}`;
    }

    let [accumulator, terminated] = executeBootCode(modifiedInstructions);
    if (terminated) {
        console.log(accumulator);
        break;
    }
}

function executeBootCode(instructions) {
    let accumulator = 0;
    let visited = {};
    let currentInstruction = 0;

    while (currentInstruction < instructions.length) {
        if (visited[currentInstruction]) {
            return [accumulator, false];
        }

        visited[currentInstruction] = true;
        let [op, arg] = parseInstruction(instructions[currentInstruction]);

        switch (op) {
            case "acc":
                accumulator += arg;
                currentInstruction++;
                break;
            case "jmp":
                currentInstruction += arg;
                break;
            case "nop":
                currentInstruction++;
                break;
        }
    }

    return [accumulator, true];
}

function parseInstruction(instruction) {
    let parts = instruction.split(' ');
    let op = parts[0];
    let arg = parseInt(parts[1]);
    return [op, arg];
}