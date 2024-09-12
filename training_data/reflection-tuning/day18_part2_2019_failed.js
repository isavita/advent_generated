function collectAllKeys(input) {
    // Trim input and split into grid
    const grid = input.trim().split('\n').map(line => line.split(''));
    
    // Find all keys and entrance positions
    const keys = new Map();
    const entrances = [];
    for (let y = 0; y < grid.length; y++) {
        for (let x = 0; x < grid[y].length; x++) {
            if (grid[y][x] === '@') {
                entrances.push([x, y]);
            } else if (grid[y][x] >= 'a' && grid[y][x] <= 'z') {
                keys.set(grid[y][x], [x, y]);
            }
        }
    }

    // BFS function to find shortest path
    function bfs(start, target) {
        const queue = [[...start, 0]];
        const visited = new Set();
        const directions = [[0, 1], [1, 0], [0, -1], [-1, 0]];

        while (queue.length > 0) {
            const [x, y, steps] = queue.shift();
            const key = `${x},${y}`;

            if (x === target[0] && y === target[1]) {
                return steps;
            }

            if (visited.has(key)) continue;
            visited.add(key);

            for (const [dx, dy] of directions) {
                const nx = x + dx;
                const ny = y + dy;
                if (nx >= 0 && nx < grid[0].length && ny >= 0 && ny < grid.length && grid[ny][nx] !== '#') {
                    queue.push([nx, ny, steps + 1]);
                }
            }
        }
        return Infinity;
    }

    // Function to collect keys
    function collectKeys(positions, collectedKeys) {
        const key = positions.map(p => p.join(',')).join('|') + '|' + [...collectedKeys].sort().join('');
        if (memo.has(key)) return memo.get(key);

        if (collectedKeys.size === keys.size) return 0;

        let minSteps = Infinity;

        for (const [key, pos] of keys) {
            if (collectedKeys.has(key)) continue;

            for (let i = 0; i < positions.length; i++) {
                const steps = bfs(positions[i], pos);
                if (steps < Infinity) {
                    const newPositions = [...positions];
                    newPositions[i] = pos;
                    const newCollectedKeys = new Set(collectedKeys);
                    newCollectedKeys.add(key);
                    const remainingSteps = collectKeys(newPositions, newCollectedKeys);
                    minSteps = Math.min(minSteps, steps + remainingSteps);
                }
            }
        }

        memo.set(key, minSteps);
        return minSteps;
    }

    const memo = new Map();
    return collectKeys(entrances, new Set());
}

// Example usage
const input = `#######
#a.#Cd#
##@#@##
#######
##@#@##
#cB#Ab#
#######`;

console.log(collectAllKeys(input));
