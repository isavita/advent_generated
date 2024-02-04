const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const grid = new Map();
let startX, startY;

input.forEach((line, y) => {
    [...line].forEach((c, x) => {
        if (c === '#') {
            grid.set(`${x},${y}`, true);
        }
    });
    startX = Math.floor(line.length / 2);
    startY = Math.floor(y / 2);
});

const dx = [0, 1, 0, -1];
const dy = [-1, 0, 1, 0];

let x = startX;
let y = startY;
let dir = 0;
let infectedCount = 0;

for (let i = 0; i < 10000; i++) {
    const pos = `${x},${y}`;
    if (grid.has(pos)) {
        dir = (dir + 1) % 4;
        grid.delete(pos);
    } else {
        dir = (dir - 1 + 4) % 4;
        grid.set(pos, true);
        infectedCount++;
    }
    x += dx[dir];
    y += dy[dir];
}

console.log(infectedCount);