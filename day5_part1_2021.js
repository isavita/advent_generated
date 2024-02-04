const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let points = {};

for (let line of input) {
    let [start, end] = line.split(' -> ');
    let [x1, y1] = start.split(',').map(Number);
    let [x2, y2] = end.split(',').map(Number);

    if (x1 === x2) {
        for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
            let point = `${x1},${y}`;
            points[point] = points[point] ? points[point] + 1 : 1;
        }
    } else if (y1 === y2) {
        for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
            let point = `${x},${y1}`;
            points[point] = points[point] ? points[point] + 1 : 1;
        }
    }
}

let count = 0;
for (let key in points) {
    if (points[key] >= 2) {
        count++;
    }
}

console.log(count);