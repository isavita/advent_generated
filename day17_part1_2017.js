const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const steps = parseInt(data);

let buffer = [0];
let currentPos = 0;

for (let i = 1; i <= 2017; i++) {
    currentPos = (currentPos + steps) % buffer.length;
    buffer.splice(currentPos + 1, 0, i);
    currentPos++;
}

const index = buffer.indexOf(2017);
console.log(buffer[(index + 1) % buffer.length]);