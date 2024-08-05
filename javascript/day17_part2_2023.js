const fs = require('fs');

class PriorityQueue {
    constructor() {
        this.items = [];
    }

    enqueue(element, priority) {
        const queueElement = { element, priority };
        if (this.isEmpty()) {
            this.items.push(queueElement);
        } else {
            let added = false;
            for (let i = 0; i < this.items.length; i++) {
                if (queueElement.priority < this.items[i].priority) {
                    this.items.splice(i, 0, queueElement);
                    added = true;
                    break;
                }
            }
            if (!added) {
                this.items.push(queueElement);
            }
        }
    }

    dequeue() {
        return this.items.shift();
    }

    isEmpty() {
        return this.items.length === 0;
    }
}

class Coord {
    constructor(x, y) {
        this.x = x;
        this.y = y;
    }

    add(c) {
        return new Coord(this.x + c.x, this.y + c.y);
    }

    subtract(c) {
        return new Coord(this.x - c.x, this.y - c.y);
    }

    opposite() {
        return new Coord(-this.x, -this.y);
    }

    equals(c) {
        return this.x === c.x && this.y === c.y;
    }
}

class Grid {
    constructor(width, height, data) {
        this.width = width;
        this.height = height;
        this.data = data;
    }

    isInBounds(coord) {
        return coord.x >= 0 && coord.x < this.width && coord.y >= 0 && coord.y < this.height;
    }

    neighbors4(coord) {
        const directions = [new Coord(0, -1), new Coord(-1, 0), new Coord(0, 1), new Coord(1, 0)];
        return directions.map(dir => coord.add(dir)).filter(neighbor => this.isInBounds(neighbor));
    }

    AStarConstrained(start, goal, minStraight, maxStraight) {
        const startInfo = { coord: start, dir: new Coord(0, 0), numStraight: 0 };
        const frontier = new PriorityQueue();
        frontier.enqueue(startInfo, 0);

        const cameFrom = {};
        const costSoFar = {};
        cameFrom[JSON.stringify(startInfo)] = startInfo;
        costSoFar[JSON.stringify(startInfo)] = 0;

        while (!frontier.isEmpty()) {
            const { element: current } = frontier.dequeue();
            const currentCost = costSoFar[JSON.stringify(current)];

            if (current.coord.x === goal.x && current.coord.y === goal.y) {
                return currentCost;
            }

            for (const next of this.neighbors4(current.coord)) {
                const newDir = next.subtract(current.coord);
                const newNumStraight = (newDir.x === current.dir.x && newDir.y === current.dir.y) ? current.numStraight + 1 : 1;
                const nextInfo = { coord: next, dir: newDir, numStraight: newNumStraight };
                const newCost = currentCost + this.data[JSON.stringify(next)];
                const actualCost = costSoFar[JSON.stringify(nextInfo)];

                const isLowerCost = typeof actualCost === 'undefined' || newCost < actualCost;
                const isValidStraight = (current.numStraight >= minStraight || newDir.x === current.dir.x && newDir.y === current.dir.y || current.coord.x === start.x && current.coord.y === start.y) && newNumStraight <= maxStraight;
                const isNotOppositeDirection = !newDir.opposite().equals(current.dir);

                if (isLowerCost && isValidStraight && isNotOppositeDirection) {
                    costSoFar[JSON.stringify(nextInfo)] = newCost;
                    cameFrom[JSON.stringify(nextInfo)] = current;
                    const priority = newCost + this.heuristic(next, goal);
                    frontier.enqueue(nextInfo, priority);
                }
            }
        }

        return -1;
    }

    heuristic(c1, c2) {
        return Math.abs(c1.x - c2.x) + Math.abs(c1.y - c2.y);
    }
}

function buildGrid(input) {
    const width = input[0].length;
    const height = input.length;
    const data = {};

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            data[JSON.stringify(new Coord(x, y))] = parseInt(input[y][x]);
        }
    }

    return new Grid(width, height, data);
}

function solve(input) {
    const grid = buildGrid(input);
    const start = new Coord(0, 0);
    const goal = new Coord(grid.width - 1, grid.height - 1);
    return grid.AStarConstrained(start, goal, 4, 10);
}

function readFile(fileName) {
    return fs.readFileSync(fileName, 'utf8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));