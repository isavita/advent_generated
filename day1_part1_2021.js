const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n').map(Number);

let prev = 0;
let count = 0;

for (let i = 0; i < data.length; i++) {
    const current = data[i];
    if (prev !== 0 && current > prev) {
        count++;
    }
    prev = current;
}

console.log(count);