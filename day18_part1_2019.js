const fs = require('fs');

class Point {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }
}

class State {
    constructor(pos, keys) {
        this.pos = pos;
        this.keys = keys;
    }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const grid = [];
let start;
const keyMap = {};
let keyCounter = 0;

for (let y = 0; y < input.length; y++) {
    const line = input[y];
    grid.push(line);
    for (let x = 0; x < line.length; x++) {
        if (line[x] === '@') {
            start = new Point(x, y);
        } else if (line[x] >= 'a' && line[x] <= 'z') {
            keyMap[line[x]] = keyCounter;
            keyCounter++;
        }
    }
}

console.log(findShortestPath(grid, start, keyMap));

function findShortestPath(grid, start, keyMap) {
    const dirs = [new Point(0, -1), new Point(-1, 0), new Point(0, 1), new Point(1, 0)];
    const visited = new Map();
    const queue = [new State(start, 0)];
    let steps = 0;

    while (queue.length > 0) {
        const size = queue.length;
        for (let i = 0; i < size; i++) {
            const current = queue.shift();

            if (current.keys === (1 << Object.keys(keyMap).length) - 1) {
                return steps;
            }

            for (const d of dirs) {
                const next = new Point(current.pos.x + d.x, current.pos.y + d.y);
                if (next.x >= 0 && next.x < grid[0].length && next.y >= 0 && next.y < grid.length) {
                    const char = grid[next.y][next.x];
                    if (char !== '#' && !(char >= 'A' && char <= 'Z' && (current.keys & (1 << keyMap[char.toLowerCase()])) === 0)) {
                        const newState = new State(next, current.keys);
                        if (char >= 'a' && char <= 'z') {
                            newState.keys |= 1 << keyMap[char];
                        }
                        const stateKey = `${newState.pos.x},${newState.pos.y},${newState.keys}`;
                        if (!visited.has(stateKey)) {
                            visited.set(stateKey, true);
                            queue.push(newState);
                        }
                    }
                }
            }
        }
        steps++;
    }
    return -1;
}