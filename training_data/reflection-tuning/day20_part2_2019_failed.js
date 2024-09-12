function solveMaze(grid, isPart2 = false) {
    const height = grid.length;
    const width = grid[0].length;
    const portals = {};
    const queue = [];
    const visited = new Set();

    // Find portals and start/end points
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            if (grid[y][x].match(/[A-Z]/)) {
                const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];
                for (const [dy, dx] of directions) {
                    const ny = y + dy;
                    const nx = x + dx;
                    if (ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] === '.') {
                        const label = grid[y][x] + (grid[y - dy] ? grid[y - dy][x - dx] : grid[y + dy][x + dx]);
                        const isOuter = y === 0 || y === height - 1 || x === 0 || x === width - 1;
                        if (label === 'AA') queue.push([ny, nx, 0, 0]);
                        else if (label !== 'ZZ') {
                            if (!portals[label]) portals[label] = [];
                            portals[label].push([ny, nx, isOuter ? 1 : -1]);
                        }
                    }
                }
            }
        }
    }

    while (queue.length > 0) {
        const [y, x, steps, level] = queue.shift();
        const key = `${y},${x},${level}`;
        if (visited.has(key)) continue;
        visited.add(key);

        if (grid[y][x] === 'Z' && (!isPart2 || level === 0)) {
            return steps - 1; // Subtract 1 because we count the start position
        }

        const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];
        for (const [dy, dx] of directions) {
            const ny = y + dy;
            const nx = x + dx;
            if (ny >= 0 && ny < height && nx >= 0 && nx < width && grid[ny][nx] === '.') {
                queue.push([ny, nx, steps + 1, level]);
            }
        }

        if (grid[y][x].match(/[A-Z]/)) {
            const label = grid[y][x] + grid[y + 1][x];
            if (portals[label]) {
                for (const [py, px, levelChange] of portals[label]) {
                    const newLevel = level + levelChange;
                    if (!isPart2 || (newLevel >= 0 && (level > 0 || label !== 'AA' && label !== 'ZZ'))) {
                        queue.push([py, px, steps + 1, newLevel]);
                    }
                }
            }
        }
    }

    return -1; // No path found
}

// Example usage:
const grid = [
    // ... (insert your maze grid here)
];

console.log(solveMaze(grid)); // Part 1
console.log(solveMaze(grid, true)); // Part 2
