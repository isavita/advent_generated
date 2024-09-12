const fs = require('fs');

function runIntcode(program, inputs) {
    // Implement the Intcode computer logic here
    // This is a placeholder and should be replaced with actual Intcode implementation
    return inputs[0] + inputs[1] > 50 ? 1 : 0;
}

function countAffectedPoints(program) {
    let affectedPoints = 0;

    for (let y = 0; y < 50; y++) {
        for (let x = 0; x < 50; x++) {
            const output = runIntcode(program, [x, y]);
            if (output === 1) {
                affectedPoints++;
            }
        }
    }

    return affectedPoints;
}

// Read the Intcode program from the input file
const input = fs.readFileSync('input.txt', 'utf8').trim();
const program = input.split(',').map(Number);

const result = countAffectedPoints(program);
console.log(result);
