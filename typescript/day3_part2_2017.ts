
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const target = parseInt(data);

const grid = new Map();
grid.set(JSON.stringify([0, 0]), 1);

let x = 0;
let y = 0;
let dx = 0;
let dy = -1;

while (true) {
    if (x === y || (x < 0 && x === -y) || (x > 0 && x === 1 - y)) {
        [dx, dy] = [-dy, dx];
    }

    x += dx;
    y += dy;

    let value = 0;
    for (let i = -1; i <= 1; i++) {
        for (let j = -1; j <= 1; j++) {
            value += grid.get(JSON.stringify([x + i, y + j])) || 0;
        }
    }
    grid.set(JSON.stringify([x, y]), value);

    if (value > target) {
        console.log(value);
        break;
    }
}
