
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

let prev = 0;
let count = 0;

for (let i = 0; i < input.length; i++) {
    const current = input[i];
    if (prev !== 0 && current > prev) {
        count++;
    }
    prev = current;
}

console.log(count);
