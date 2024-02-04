const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

let freq = 0;
for (let i = 0; i < input.length; i++) {
    const change = input[i];
    const [sign, num] = parseChange(change);
    freq += sign * num;
}

console.log(freq);

function parseChange(change) {
    let sign = 1;
    if (change[0] === '-') {
        sign = -1;
        change = change.slice(1);
    }
    const num = parseInt(change, 10);
    return [sign, num];
}