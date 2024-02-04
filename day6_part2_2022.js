const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
console.log(firstNUnique(data, 14));

function firstNUnique(s, n) {
    for (let i = n; i < s.length; i++) {
        const b = s.slice(i - n, i).split('');
        if (b.length === new Set(b).size) {
            return i;
        }
    }
    return -1;
}