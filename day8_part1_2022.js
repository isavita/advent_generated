const fs = require('fs');

const Neighbors4 = [{ x: 0, y: 1 }, { x: 0, y: -1 }, { x: 1, y: 0 }, { x: -1, y: 0 }];

const grid = {};
const visible = {};

const data = fs.readFileSync('input.txt', 'utf8').split('\n');
let y = 0;
data.forEach(line => {
    [...line].forEach((char, x) => {
        grid[`${x},${y}`] = parseInt(char);
    });
    y++;
});

Object.keys(grid).forEach(point => {
    const [x, y] = point.split(',').map(Number);
    Neighbors4.forEach(neighbor => {
        let next = { x, y };
        while (true) {
            next.x += neighbor.x;
            next.y += neighbor.y;
            const nextPoint = `${next.x},${next.y}`;
            if (grid[nextPoint] !== undefined) {
                if (grid[nextPoint] >= grid[point]) {
                    break;
                }
            } else {
                visible[point] = true;
                break;
            }
        }
    });
});

console.log(Object.keys(visible).length);