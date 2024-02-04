const fs = require('fs');

function readAll(path) {
    return fs.readFileSync(path, 'utf8');
}

function compare(a, b) {
    if (typeof a === 'number' && typeof b === 'number') {
        return Math.sign(a - b);
    } else if (typeof a === 'number') {
        return compare([a], b);
    } else if (typeof b === 'number') {
        return compare(a, [b]);
    } else {
        for (let i = 0; i < Math.min(a.length, b.length); i++) {
            const c = compare(a[i], b[i]);
            if (c !== 0) {
                return c;
            }
        }
        return Math.sign(a.length - b.length);
    }
}

const input = readAll('input.txt');
const packets = [];
let sum = 0;

input.split('\n\n').forEach((pair, i) => {
    const sp = pair.split('\n');
    const first = JSON.parse(sp[0]);
    const second = JSON.parse(sp[1]);
    packets.push(first, second);
    if (compare(first, second) === -1) {
        sum += i + 1;
    }
});

console.log(sum);