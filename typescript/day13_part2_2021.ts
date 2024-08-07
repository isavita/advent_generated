import * as fs from 'fs';
import * as readline from 'readline';

const fileStream = fs.createReadStream('input.txt');
const rl = readline.createInterface({ input: fileStream, crlfDelay: Infinity });

const points: Set<string> = new Set();
const folds: [number, number][] = [];
let readingPoints = true;

rl.on('line', (line) => {
    if (line === '') {
        readingPoints = false;
        return;
    }
    if (readingPoints) {
        const [x, y] = line.split(',').map(Number);
        points.add(`${x},${y}`);
    } else {
        const [axis, val] = line.split('=');
        const value = parseInt(val, 10);
        if (axis.includes('x')) {
            folds.push([value, 0]);
        } else {
            folds.push([0, value]);
        }
    }
});

rl.on('close', () => {
    folds.forEach((fold, i) => {
        const newPoints: Set<string> = new Set();
        points.forEach(point => {
            let [x, y] = point.split(',').map(Number);
            if (fold[0] !== 0 && x > fold[0]) {
                x = fold[0] - (x - fold[0]);
            } else if (fold[1] !== 0 && y > fold[1]) {
                y = fold[1] - (y - fold[1]);
            }
            newPoints.add(`${x},${y}`);
        });
        points.clear();
        newPoints.forEach(point => points.add(point));
        if (i === 0) {
            console.log("Number of dots visible after first fold:", points.size);
        }
    });

    let maxX = 0, maxY = 0;
    points.forEach(point => {
        const [x, y] = point.split(',').map(Number);
        if (x > maxX) maxX = x;
        if (y > maxY) maxY = y;
    });

    const grid: string[][] = Array.from({ length: maxY + 1 }, () => Array(maxX + 1).fill(' '));
    points.forEach(point => {
        const [x, y] = point.split(',').map(Number);
        grid[y][x] = '#';
    });

    grid.forEach(row => console.log(row.join('')));
});