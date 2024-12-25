
const fs = require('fs');

const size = 71;
const grid = Array(size).fill(null).map(() => Array(size).fill(false));

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
for (let i = 0; i < 1024 && i < input.length; i++) {
    const [x, y] = input[i].split(',').map(Number);
    if (x >= 0 && x < size && y >= 0 && y < size) {
        grid[y][x] = true;
    }
}

const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
const visited = Array(size).fill(null).map(() => Array(size).fill(false));

const q = [{ x: 0, y: 0, steps: 0 }];
visited[0][0] = true;

while (q.length > 0) {
    const { x, y, steps } = q.shift();
    if (x === size - 1 && y === size - 1) {
        console.log(steps);
        process.exit(0);
    }
    for (const [dx, dy] of dirs) {
        const nx = x + dx;
        const ny = y + dy;
        if (nx >= 0 && ny >= 0 && nx < size && ny < size && !grid[ny][nx] && !visited[ny][nx]) {
            visited[ny][nx] = true;
            q.push({ x: nx, y: ny, steps: steps + 1 });
        }
    }
}

console.log("No path");
