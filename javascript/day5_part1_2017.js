const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');
const offsets = lines.map(Number);

let index = 0;
let steps = 0;

while (index >= 0 && index < offsets.length) {
    const jump = offsets[index];
    offsets[index]++;
    index += jump;
    steps++;
}

console.log(steps);