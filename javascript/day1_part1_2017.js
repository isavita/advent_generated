const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
let sum = 0;

for (let i = 0; i < data.length; i++) {
    const next = (i + 1) % data.length;
    if (data[i] === data[next]) {
        sum += parseInt(data[i]);
    }
}

console.log(sum);