const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let count = 0;
for (let i = 0; i < input.length; i++) {
    const pair = input[i].split(',');

    const left = parseRange(pair[0]);
    const right = parseRange(pair[1]);

    if (left[0] <= right[1] && left[1] >= right[0]) {
        count++;
    }
}

console.log(count);

function parseRange(s) {
    const split = s.split('-');
    const start = parseInt(split[0]);
    const end = parseInt(split[1]);
    return [start, end];
}