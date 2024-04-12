const fs = require('fs');

class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    equals(other) {
        return this.x === other.x && this.y === other.y;
    }

    toString() {
        return `${this.x},${this.y}`;
    }
}

function buildMap(regex) {
    const doorMap = {};
    let stack = [];
    let cp = new Point(0, 0);

    for (const c of regex) {
        switch (c) {
            case '(':
                stack.push(cp);
                break;
            case '|':
                cp = stack[stack.length - 1];
                break;
            case ')':
                cp = stack.pop();
                break;
            case 'N':
            case 'S':
            case 'E':
            case 'W':
                const np = move(cp, c);
                if (!doorMap[cp]) {
                    doorMap[cp] = {};
                }
                doorMap[cp][np] = true;
                cp = np;
                break;
        }
    }

    return doorMap;
}

function move(p, dir) {
    switch (dir) {
        case 'N':
            return new Point(p.x, p.y - 1);
        case 'S':
            return new Point(p.x, p.y + 1);
        case 'E':
            return new Point(p.x + 1, p.y);
        case 'W':
            return new Point(p.x - 1, p.y);
    }
}

function findFurthestRoom(doorMap) {
    const visited = {};
    const queue = [new Point(0, 0)];
    visited[new Point(0, 0)] = 0;
    let maxDoors = 0;

    while (queue.length > 0) {
        const p = queue.shift();

        if (doorMap[p]) {
            for (const npStr in doorMap[p]) {
                const np = Point.fromString(npStr);
                if (!visited[np]) {
                    visited[np] = visited[p] + 1;
                    maxDoors = Math.max(maxDoors, visited[np]);
                    queue.push(np);
                }
            }
        }
    }

    return maxDoors;
}

Point.fromString = function (str) {
    const [x, y] = str.split(',').map(Number);
    return new Point(x, y);
};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const regex = data.trim().slice(1, -1);
    const doorMap = buildMap(regex);
    const maxDoors = findFurthestRoom(doorMap);
    console.log(maxDoors);
});