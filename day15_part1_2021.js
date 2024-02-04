const fs = require('fs');

class Position {
    constructor(x, y, risk) {
        this.x = x;
        this.y = y;
        this.risk = risk;
    }
}

class PriorityQueue {
    constructor() {
        this.data = [];
    }

    push(item) {
        this.data.push(item);
        this.data.sort((a, b) => a.risk - b.risk);
    }

    pop() {
        return this.data.shift();
    }

    get length() {
        return this.data.length;
    }
}

function dijkstra(grid) {
    const pq = new PriorityQueue();
    pq.push(new Position(0, 0, 0));

    const rows = grid.length;
    const cols = grid[0].length;
    const dist = new Array(rows).fill().map(() => new Array(cols).fill(1 << 31 - 1));

    dist[0][0] = 0;

    const directions = [new Position(1, 0, 0), new Position(0, 1, 0), new Position(-1, 0, 0), new Position(0, -1, 0)];

    while (pq.length > 0) {
        const curr = pq.pop();
        if (curr.x === rows - 1 && curr.y === cols - 1) {
            return curr.risk;
        }
        for (const d of directions) {
            const nx = curr.x + d.x;
            const ny = curr.y + d.y;
            if (nx >= 0 && ny >= 0 && nx < rows && ny < cols) {
                const nextRisk = curr.risk + grid[nx][ny];
                if (nextRisk < dist[nx][ny]) {
                    dist[nx][ny] = nextRisk;
                    pq.push(new Position(nx, ny, nextRisk));
                }
            }
        }
    }
    return -1;
}

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.split('').map(Number));
console.log(dijkstra(input));