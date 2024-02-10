const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.trim().split('\n');

const holderMap = {};
const heldMap = {};

const re = /[a-z]+/g;

lines.forEach(line => {
    const names = line.match(re);
    const holder = names[0];
    holderMap[holder] = true;

    if (names.length > 1) {
        names.slice(1).forEach(name => {
            heldMap[name] = true;
        });
    }
});

for (const holder in holderMap) {
    if (!heldMap[holder]) {
        console.log(holder);
        break;
    }
}