const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let x = [1];
for (const line of input) {
    switch (line) {
        case "noop":
            x.push(x[x.length - 1]);
            break;
        default:
            const n = parseInt(line.split(' ')[1]);
            x.push(x[x.length - 1]);
            x.push(x[x.length - 1] + n);
            break;
    }
}

let sum = 0;
for (let i = 0; i < x.length; i++) {
    if ((i - 19) % 40 === 0) {
        sum += (i + 1) * x[i];
    }
}

console.log(sum);