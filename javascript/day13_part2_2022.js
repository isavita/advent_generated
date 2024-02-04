const fs = require('fs');

const compare = (a, b) => {
    const isNumber = (value) => typeof value === 'number';
    
    if (isNumber(a) && isNumber(b)) {
        return Math.sign(a - b);
    } else if (isNumber(a)) {
        return compare([a], b);
    } else if (isNumber(b)) {
        return compare(a, [b]);
    } else {
        for (let i = 0; i < Math.min(a.length, b.length); i++) {
            const result = compare(a[i], b[i]);
            if (result !== 0) {
                return result;
            }
        }
        return Math.sign(a.length - b.length);
    }
};

const readAll = (path) => {
    return fs.readFileSync(path, 'utf8');
};

const input = readAll('input.txt');
const packets = [];
const pairs = input.split('\n\n');

pairs.forEach(pair => {
    const [first, second] = pair.split('\n');
    packets.push(JSON.parse(first), JSON.parse(second));
});

const divider1 = [[2]];
const divider2 = [[6]];
packets.push(divider1, divider2);

packets.sort((a, b) => compare(a, b));
const divider1Pos = packets.findIndex(packet => compare(packet, divider1) >= 0);
const divider2Pos = packets.findIndex(packet => compare(packet, divider2) >= 0);

console.log((divider1Pos + 1) * (divider2Pos + 1));