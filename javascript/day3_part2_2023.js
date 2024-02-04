const fs = require('fs');

const Neighbors8 = [
    [0, 1], [0, -1], [1, 0], [-1, 0],
    [-1, -1], [-1, 1], [1, -1], [1, 1],
];

class Part {
    constructor(xmin, xmax, y, n) {
        this.xmin = xmin;
        this.xmax = xmax;
        this.y = y;
        this.n = n;
    }

    valid(grid) {
        for (let x = this.xmin; x <= this.xmax; x++) {
            for (let n of Neighbors8) {
                const [dx, dy] = n;
                const key = `${x + dx},${this.y + dy}`;
                if (grid[key] && grid[key] !== '.' && (grid[key] < '0' || grid[key] > '9')) {
                    return true;
                }
            }
        }
        return false;
    }
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
const grid = {};
const parts = [];
let curr = null;

input.split('\n').forEach((line, y) => {
    if (curr) {
        parts.push(curr);
        curr = null;
    }
    for (let x = 0; x < line.length; x++) {
        const c = line[x];
        const key = `${x},${y}`;
        grid[key] = c;
        if (c >= '0' && c <= '9') {
            if (!curr) {
                curr = new Part(x, x, y, parseInt(c));
            } else {
                curr.n *= 10;
                curr.n += parseInt(c);
                curr.xmax = x;
            }
        } else if (curr) {
            parts.push(curr);
            curr = null;
        }
    }
});

const partsGrid = {};
parts.forEach((p, i) => {
    for (let x = p.xmin; x <= p.xmax; x++) {
        partsGrid[`${x},${p.y}`] = i;
    }
});

let sum = 0;
for (let p in grid) {
    if (grid[p] === '*') {
        const neighborParts = new Set();
        for (let n of Neighbors8) {
            const [dx, dy] = n;
            const key = `${parseInt(p.split(',')[0]) + dx},${parseInt(p.split(',')[1]) + dy}`;
            if (partsGrid[key] !== undefined) {
                neighborParts.add(partsGrid[key]);
            }
        }
        if (neighborParts.size === 2) {
            let prod = 1;
            neighborParts.forEach(i => {
                prod *= parts[i].n;
            });
            sum += prod;
        }
    }
}
console.log(sum);