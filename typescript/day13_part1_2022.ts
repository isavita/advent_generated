
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
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

function compare(a, b) {
    const anum = typeof a === 'number';
    const bnum = typeof b === 'number';

    if (anum && bnum) {
        return sign(a - b);
    } else if (anum) {
        return compare([a], b);
    } else if (bnum) {
        return compare(a, [b]);
    } else {
        const aa = a;
        const bb = b;

        for (let i = 0; i < aa.length && i < bb.length; i++) {
            const c = compare(aa[i], bb[i]);
            if (c !== 0) {
                return c;
            }
        }

        return sign(aa.length - bb.length);
    }
}

function sign(n) {
    if (n === 0) {
        return 0;
    }
    return n < 0 ? -1 : 1;
}
