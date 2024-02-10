const fs = require('fs');

const Clean = 0;
const Weakened = 1;
const Infected = 2;
const Flagged = 3;

const dx = [0, 1, 0, -1];
const dy = [-1, 0, 1, 0];

const grid = new Map();
let startX, startY;

const data = fs.readFileSync('input.txt', 'utf8').split('\n');
data.forEach((line, y) => {
    line.split('').forEach((c, x) => {
        if (c === '#') {
            grid.set(`${x},${y}`, Infected);
        }
    });
    startX = Math.floor(line.length / 2);
    startY = Math.floor(y / 2);
});

let x = startX;
let y = startY;
let dir = 0;
let infectedCount = 0;

for (let i = 0; i < 10000000; i++) {
    const pos = `${x},${y}`;
    switch (grid.get(pos) || Clean) {
        case Clean:
            dir = (dir - 1 + 4) % 4;
            grid.set(pos, Weakened);
            break;
        case Weakened:
            grid.set(pos, Infected);
            infectedCount++;
            break;
        case Infected:
            dir = (dir + 1) % 4;
            grid.set(pos, Flagged);
            break;
        case Flagged:
            dir = (dir + 2) % 4;
            grid.set(pos, Clean);
            break;
    }
    x += dx[dir];
    y += dy[dir];
}

console.log(infectedCount);