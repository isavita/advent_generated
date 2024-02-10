const fs = require('fs');

class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

function isWall(favoriteNumber, x, y) {
    let num = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    let bits = 0;
    while (num > 0) {
        if (num % 2 === 1) {
            bits++;
        }
        num = Math.floor(num / 2);
    }
    return bits % 2 !== 0;
}

function bfs(start, target, favoriteNumber) {
    let visited = new Map();
    let queue = [start];
    let steps = 0;

    while (queue.length > 0) {
        let size = queue.length;
        for (let i = 0; i < size; i++) {
            let point = queue[i];
            if (point.x === target.x && point.y === target.y) {
                return steps;
            }

            let deltas = [new Point(1, 0), new Point(-1, 0), new Point(0, 1), new Point(0, -1)];
            for (let delta of deltas) {
                let next = new Point(point.x + delta.x, point.y + delta.y);
                if (next.x >= 0 && next.y >= 0 && !isWall(favoriteNumber, next.x, next.y) && !visited.has(JSON.stringify(next))) {
                    visited.set(JSON.stringify(next), true);
                    queue.push(next);
                }
            }
        }
        queue = queue.slice(size);
        steps++;
    }

    return -1;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const num = data.trim();
    const favoriteNumber = parseInt(num);
    const start = new Point(1, 1);
    const target = new Point(31, 39);
    const steps = bfs(start, target, favoriteNumber);
    console.log(steps);
});