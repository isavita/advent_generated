const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');

const offsets = lines.map(line => parseInt(line));

let index = 0;
let steps = 0;

while (index >= 0 && index < offsets.length) {
    const jump = offsets[index];

    if (jump >= 3) {
        offsets[index]--;
    } else {
        offsets[index]++;
    }

    index += jump;
    steps++;
}

console.log(steps);