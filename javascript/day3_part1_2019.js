const fs = require('fs');

class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

function getPoints(path) {
    const points = new Map();
    let current = new Point(0, 0);
    path.split(',').forEach(move => {
        const dir = move[0];
        const steps = parseInt(move.slice(1));
        for (let i = 0; i < steps; i++) {
            switch (dir) {
                case 'U':
                    current.y++;
                    break;
                case 'D':
                    current.y--;
                    break;
                case 'L':
                    current.x--;
                    break;
                case 'R':
                    current.x++;
                    break;
            }
            points.set(JSON.stringify(new Point(current.x, current.y)), true);
        }
    });
    return points;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const lines = data.trim().split('\n');
    const wire1 = getPoints(lines[0]);
    const wire2 = getPoints(lines[1]);

    const intersections = new Map();
    for (const [key] of wire1) {
        if (wire2.has(key)) {
            intersections.set(key, true);
        }
    }

    let minDistance = Number.MAX_SAFE_INTEGER;
    for (const [key] of intersections) {
        const point = JSON.parse(key);
        const distance = Math.abs(point.x) + Math.abs(point.y);
        if (distance < minDistance) {
            minDistance = distance;
        }
    }

    console.log(minDistance);
});