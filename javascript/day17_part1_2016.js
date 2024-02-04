const fs = require('fs');

class Point {
    constructor(x, y, path) {
        this.x = x;
        this.y = y;
        this.path = path;
    }
}

function readPasscode(filename) {
    const data = fs.readFileSync(filename, 'utf8');
    return data.trim();
}

function findShortestPath(passcode) {
    const queue = [new Point(0, 0, "")];
    while (queue.length > 0) {
        const point = queue.shift();

        if (point.x === 3 && point.y === 3) {
            return point.path;
        }

        const openDoors = getOpenDoors(passcode, point.path);
        for (const dir of openDoors) {
            let nextPoint = new Point(point.x, point.y, point.path + dir);
            switch (dir) {
                case "U":
                    nextPoint.y--;
                    break;
                case "D":
                    nextPoint.y++;
                    break;
                case "L":
                    nextPoint.x--;
                    break;
                case "R":
                    nextPoint.x++;
                    break;
            }

            if (nextPoint.x >= 0 && nextPoint.x < 4 && nextPoint.y >= 0 && nextPoint.y < 4) {
                queue.push(nextPoint);
            }
        }
    }
    return "No path found";
}

function getOpenDoors(passcode, path) {
    const hash = md5Hash(passcode + path);
    const doors = [];
    if (hash[0] >= 'b' && hash[0] <= 'f') {
        doors.push("U");
    }
    if (hash[1] >= 'b' && hash[1] <= 'f') {
        doors.push("D");
    }
    if (hash[2] >= 'b' && hash[2] <= 'f') {
        doors.push("L");
    }
    if (hash[3] >= 'b' && hash[3] <= 'f') {
        doors.push("R");
    }
    return doors;
}

function md5Hash(input) {
    const crypto = require('crypto');
    const hash = crypto.createHash('md5');
    hash.update(input);
    return hash.digest('hex');
}

const passcode = readPasscode("input.txt");
const path = findShortestPath(passcode);
console.log(path);