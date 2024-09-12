const fs = require('fs');

function solveMaze(input) {
    const grid = input.split('\n').map(line => line.split(''));
    const height = grid.length;
    const width = grid[0].length;

    // Find all portals and walkable positions
    const portals = new Map();
    const walkable = new Set();
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            if (grid[y][x] === '.') {
                walkable.add(`${x},${y}`);
                // Check for adjacent portal labels
                [[-1,0], [1,0], [0,-1], [0,1]].forEach(([dx, dy]) => {
                    const nx = x + dx, ny = y + dy;
                    if (nx >= 0 && nx < width && ny >= 0 && ny < height && grid[ny][nx].match(/[A-Z]/)) {
                        const nnx = nx + dx, nny = ny + dy;
                        const label = dx + dy > 0 ? grid[ny][nx] + grid[nny][nnx] : grid[nny][nnx] + grid[ny][nx];
                        if (!portals.has(label)) portals.set(label, []);
                        portals.get(label).push(`${x},${y}`);
                    }
                });
            }
        }
    }

    // BFS
    const start = portals.get('AA')[0];
    const end = portals.get('ZZ')[0];
    const queue = [[start, 0]];
    const visited = new Set([start]);

    while (queue.length > 0) {
        const [pos, steps] = queue.shift();
        if (pos === end) return steps;

        const [x, y] = pos.split(',').map(Number);
        // Check adjacent positions
        [[-1,0], [1,0], [0,-1], [0,1]].forEach(([dx, dy]) => {
            const newPos = `${x+dx},${y+dy}`;
            if (walkable.has(newPos) && !visited.has(newPos)) {
                visited.add(newPos);
                queue.push([newPos, steps + 1]);
            }
        });

        // Check for portal teleportation
        for (let [label, positions] of portals) {
            if (label !== 'AA' && label !== 'ZZ' && positions.includes(pos)) {
                const newPos = positions.find(p => p !== pos);
                if (!visited.has(newPos)) {
                    visited.add(newPos);
                    queue.push([newPos, steps + 1]);
                }
            }
        }
    }

    return -1; // No path found
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(solveMaze(input));
