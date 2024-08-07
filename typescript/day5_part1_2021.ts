import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.trim().split('\n');

interface Point {
    x: number;
    y: number;
}

const grid: number[][] = Array.from({ length: 1000 }, () => Array(1000).fill(0));

lines.forEach(line => {
    const [start, end] = line.split(' -> ').map(coord => coord.split(',').map(Number));
    const [x1, y1] = start;
    const [x2, y2] = end;

    if (x1 === x2) { // Vertical line
        const minY = Math.min(y1, y2);
        const maxY = Math.max(y1, y2);
        for (let y = minY; y <= maxY; y++) {
            grid[x1][y]++;
        }
    } else if (y1 === y2) { // Horizontal line
        const minX = Math.min(x1, x2);
        const maxX = Math.max(x1, x2);
        for (let x = minX; x <= maxX; x++) {
            grid[x][y1]++;
        }
    }
});

const overlapCount = grid.flat().filter(count => count >= 2).length;

console.log(overlapCount);