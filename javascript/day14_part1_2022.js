const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const grid = new Map();
input.forEach(line => {
    const points = line.split(' -> ').map(point => point.split(',').map(Number));
    for (let i = 0; i < points.length - 1; i++) {
        const [x1, y1] = points[i];
        const [x2, y2] = points[i + 1];
        if (x1 === x2) {
            for (let y = Math.min(y1, y2); y <= Math.max(y1, y2); y++) {
                grid.set(`${x1},${y}`, true);
            }
        } else {
            for (let x = Math.min(x1, x2); x <= Math.max(x1, x2); x++) {
                grid.set(`${x},${y1}`, true);
            }
        }
    }
});

console.log(fill(grid));

function fill(grid) {
    const floor = bounds(grid).max.y + 1;
    let sands = 0;
    let firstFloorTouch = 0;
    let full = false;
    while (!full) {
        if (grid.has('500,0')) {
            full = true;
        } else {
            let sand = [500, 0];
            let settled = false;
            while (!settled) {
                const [x, y] = sand;
                if (y === floor) {
                    if (firstFloorTouch === 0) {
                        firstFloorTouch = sands;
                    }
                    grid.set(`${x},${y}`, true);
                    break;
                }
                let nextSand = null;
                for (const [dx, dy] of [[0, 1], [-1, 1], [1, 1]]) {
                    const nx = x + dx;
                    const ny = y + dy;
                    if (!grid.has(`${nx},${ny}`)) {
                        nextSand = [nx, ny];
                        break;
                    }
                }
                if (nextSand) {
                    sand = nextSand;
                } else {
                    grid.set(`${x},${y}`, true);
                    settled = true;
                }
            }
            sands++;
        }
    }
    return firstFloorTouch;
}

function bounds(grid) {
    const points = Array.from(grid.keys()).map(point => point.split(',').map(Number));
    return boundss(points);
}

function boundss(points) {
    let minX = Infinity;
    let minY = Infinity;
    let maxX = -Infinity;
    let maxY = -Infinity;
    points.forEach(([x, y]) => {
        minX = Math.min(minX, x);
        minY = Math.min(minY, y);
        maxX = Math.max(maxX, x);
        maxY = Math.max(maxY, y);
    });
    return { min: { x: minX, y: minY }, max: { x: maxX, y: maxY } };
}