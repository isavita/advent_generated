const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const target = parseInt(data);

const grid = new Map();
grid.set([0, 0].toString(), 1);

let x = 0, y = 0;
let dx = 0, dy = -1;

while (true) {
    if (x === y || (x < 0 && x === -y) || (x > 0 && x === 1 - y)) {
        [dx, dy] = [-dy, dx];
    }

    x += dx;
    y += dy;

    let value = 0;
    for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
            value += grid.get([x + dx, y + dy].toString()) || 0;
        }
    }
    grid.set([x, y].toString(), value);

    if (value > target) {
        console.log(value);
        break;
    }
}