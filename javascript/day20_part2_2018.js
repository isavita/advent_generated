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
    const stack = [];
    let currentPoint = new Point(0, 0);

    for (const c of regex) {
        if (c === '(') {
            stack.push(currentPoint);
        } else if (c === '|') {
            currentPoint = stack[stack.length - 1];
        } else if (c === ')') {
            currentPoint = stack.pop();
        } else {
            const nextPoint = move(currentPoint, c);
            if (!doorMap[currentPoint.toString()]) {
                doorMap[currentPoint.toString()] = {};
            }
            doorMap[currentPoint.toString()][nextPoint.toString()] = true;
            currentPoint = nextPoint;
        }
    }
    return doorMap;
}

function move(point, direction) {
    switch (direction) {
        case 'N':
            return new Point(point.x, point.y - 1);
        case 'S':
            return new Point(point.x, point.y + 1);
        case 'E':
            return new Point(point.x + 1, point.y);
        case 'W':
            return new Point(point.x - 1, point.y);
    }
    return point;
}

function countRooms(doorMap, minDoors) {
    const visited = {};
    const queue = [new Point(0, 0)];
    let roomCount = 0;

    while (queue.length > 0) {
        const point = queue.shift();
        for (const nextPointStr in doorMap[point.toString()]) {
            if (!visited[nextPointStr]) {
                const nextPoint = strToPoint(nextPointStr);
                visited[nextPointStr] = (visited[point.toString()] || 0) + 1;
                if (visited[nextPointStr] >= minDoors) {
                    roomCount++;
                }
                queue.push(nextPoint);
            }
        }
    }
    return roomCount;
}

function strToPoint(str) {
    const [x, y] = str.split(',').map(Number);
    return new Point(x, y);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const regex = data.trim().slice(1, -1);
    const doorMap = buildMap(regex);
    const rooms = countRooms(doorMap, 1000);
    console.log(rooms);
});