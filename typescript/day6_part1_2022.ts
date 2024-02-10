const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
console.log(firstNUnique(data, 4));

function firstNUnique(s, n) {
    for (let i = n; i < s.length; i++) {
        const b = Array.from(new Set(s.slice(i - n, i)));
        if (b.length === n) {
            return i;
        }
    }
    return -1;
}