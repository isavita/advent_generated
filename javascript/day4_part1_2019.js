const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n')[0].split('-');
const start = parseInt(input[0]);
const end = parseInt(input[1]);

let count = 0;
for (let i = start; i <= end; i++) {
    const s = i.toString();
    if (hasDoubleAndIncreasingDigits(s)) {
        count++;
    }
}

console.log(count);

function hasDoubleAndIncreasingDigits(s) {
    let hasDouble = false;
    for (let i = 0; i < s.length - 1; i++) {
        if (s[i] === s[i + 1]) {
            hasDouble = true;
        }
        if (s[i] > s[i + 1]) {
            return false;
        }
    }
    return hasDouble;
}