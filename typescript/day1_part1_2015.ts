const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
let floor = 0;
for (const c of input) {
    if (c === '(') {
        floor++;
    } else if (c === ')') {
        floor--;
    }
}
console.log(floor);