const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
let floor = 0;
let position = 0;

for (let i = 0; i < input.length; i++) {
    if (input[i] === '(') {
        floor++;
    } else if (input[i] === ')') {
        floor--;
    }
    if (floor === -1) {
        position = i + 1;
        break;
    }
}

console.log(position);