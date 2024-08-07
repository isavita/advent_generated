import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8');

const grid = new Map<string, string>();
const parts: [number, number, number, number][] = [];

for (const [y, line] of input.trim().split('\n').entries()) {
    let curr: [number, number, number, number] | null = null;
    for (const [x, c] of line.split('').entries()) {
        const key = `${x},${y}`;
        grid.set(key, c);
        if (!isNaN(parseInt(c))) {
            if (curr === null) {
                curr = [y, x, x, parseInt(c)];
            } else {
                curr[3] = (curr[3] * 10) + parseInt(c);
                curr[2] = x;
            }
        } else if (curr !== null) {
            parts.push(curr);
            curr = null;
        }
    }
    if (curr !== null) {
        parts.push(curr);
        curr = null;
    }
}

const partsGrid = new Map<string, number>();
for (const [i, part] of parts.entries()) {
    for (let x = part[1]; x <= part[2]; x++) {
        const key = `${x},${part[0]}`;
        partsGrid.set(key, i);
    }
}

let sum = 0;
for (const [p, c] of grid.entries()) {
    if (c === '*') {
        const [x, y] = p.split(',').map(Number);
        const neighbors: string[] = [
            `${x-1},${y-1}`,
            `${x-1},${y}`,
            `${x-1},${y+1}`,
            `${x},${y-1}`,
            `${x},${y+1}`,
            `${x+1},${y-1}`,
            `${x+1},${y}`,
            `${x+1},${y+1}`,
        ].filter(p => partsGrid.has(p));
        const s = new Set(neighbors.map(p => partsGrid.get(p)));
        if (s.size === 2) {
            const product = [...s].map(i => parts[i!][3]).reduce((a, b) => a * b, 1);
            sum += product;
        }
    }
}

console.log(sum);