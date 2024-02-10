const fs = require('fs');

class Position {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

function firstRevisitedDistance(instructions) {
    let pos = new Position(0, 0);
    let visited = new Map();
    visited.set(JSON.stringify(pos), true);
    let directions = [new Position(0, 1), new Position(1, 0), new Position(0, -1), new Position(-1, 0)];
    let dirIndex = 0;

    for (let instruction of instructions) {
        let turn = instruction.slice(0, 1);
        let blocks = parseInt(instruction.slice(1));

        if (turn === "R") {
            dirIndex = (dirIndex + 1) % 4;
        } else {
            dirIndex = (dirIndex - 1 + 4) % 4;
        }

        for (let i = 0; i < blocks; i++) {
            pos.x += directions[dirIndex].x;
            pos.y += directions[dirIndex].y;

            if (visited.has(JSON.stringify(pos))) {
                return Math.abs(pos.x) + Math.abs(pos.y);
            }
            visited.set(JSON.stringify(pos), true);
        }
    }

    return -1;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const instructions = data.trim().split(', ');

    console.log(firstRevisitedDistance(instructions));
});