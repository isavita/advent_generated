// Read input from input.txt
const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').split('\n');

// Parse the maze
function parseMaze(input) {
    const maze = new Map();
    const portals = new Map();
    const width = Math.max(...input.map(line => line.length));
    const height = input.length;

    // Read maze into 2D grid
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const char = input[y][x] || ' ';
            if (char !== ' ') {
                maze.set(`${x},${y}`, char);
            }
        }
    }

    // Find portals
    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            if (/[A-Z]/.test(input[y]?.[x] || '')) {
                // Check horizontal portal
                if (/[A-Z]/.test(input[y]?.[x + 1] || '')) {
                    const name = input[y][x] + input[y][x + 1];
                    const isOuter = x === 0 || x + 2 >= width - 2;
                    
                    if (maze.get(`${x-1},${y}`) === '.') {
                        portals.set(`${x-1},${y}`, {name, isOuter});
                    } else if (maze.get(`${x+2},${y}`) === '.') {
                        portals.set(`${x+2},${y}`, {name, isOuter});
                    }
                }
                // Check vertical portal
                if (/[A-Z]/.test(input[y + 1]?.[x] || '')) {
                    const name = input[y][x] + input[y + 1][x];
                    const isOuter = y === 0 || y + 2 >= height - 2;
                    
                    if (maze.get(`${x},${y-1}`) === '.') {
                        portals.set(`${x},${y-1}`, {name, isOuter});
                    } else if (maze.get(`${x},${y+2}`) === '.') {
                        portals.set(`${x},${y+2}`, {name, isOuter});
                    }
                }
            }
        }
    }

    return {maze, portals};
}

// Find shortest path considering recursion
function findShortestPath(maze, portals) {
    const start = [...portals.entries()].find(([_, p]) => p.name === 'AA')[0];
    const end = [...portals.entries()].find(([_, p]) => p.name === 'ZZ')[0];
    
    const portalsByName = new Map();
    for (const [pos, portal] of portals) {
        if (!portalsByName.has(portal.name)) {
            portalsByName.set(portal.name, []);
        }
        portalsByName.get(portal.name).push({pos, isOuter: portal.isOuter});
    }

    const queue = [{pos: start, level: 0, steps: 0}];
    const seen = new Set();
    const dirs = [[0,1], [1,0], [0,-1], [-1,0]];

    while (queue.length > 0) {
        const {pos, level, steps} = queue.shift();
        const key = `${pos},${level}`;
        
        if (seen.has(key)) continue;
        seen.add(key);

        // Found the exit at level 0
        if (pos === end && level === 0) {
            return steps;
        }

        // Try normal moves
        const [x, y] = pos.split(',').map(Number);
        for (const [dx, dy] of dirs) {
            const newPos = `${x+dx},${y+dy}`;
            if (maze.get(newPos) === '.') {
                queue.push({pos: newPos, level, steps: steps + 1});
            }
        }

        // Try portal moves
        if (portals.has(pos)) {
            const portal = portals.get(pos);
            if (portal.name !== 'AA' && portal.name !== 'ZZ') {
                const [other] = portalsByName.get(portal.name)
                    .filter(p => p.pos !== pos);
                
                // Outer portals decrease level, inner portals increase level
                const newLevel = level + (portal.isOuter ? -1 : 1);
                
                // Only use portal if we don't go below level 0
                if (newLevel >= 0) {
                    queue.push({
                        pos: other.pos,
                        level: newLevel,
                        steps: steps + 1
                    });
                }
            }
        }
    }

    return -1; // No path found
}

// Main execution
const {maze, portals} = parseMaze(input);
const steps = findShortestPath(maze, portals);
console.log(steps);
