const fs = require('fs');

function parseInput(input) {
    const clay = new Set();
    let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;

    input.split('\n').forEach(line => {
        const [a, b] = line.split(', ');
        const [axis1, value1] = a.split('=');
        const [axis2, range] = b.split('=');
        const [start, end] = range.split('..').map(Number);

        for (let i = start; i <= end; i++) {
            const x = axis1 === 'x' ? Number(value1) : i;
            const y = axis1 === 'y' ? Number(value1) : i;
            clay.add(`${x},${y}`);
            minX = Math.min(minX, x);
            maxX = Math.max(maxX, x);
            minY = Math.min(minY, y);
            maxY = Math.max(maxY, y);
        }
    });

    return { clay, minX, maxX, minY, maxY };
}

function simulateWater(clay, minX, maxX, minY, maxY) {
    const water = new Set();
    const rest = new Set();

    function flow(x, y) {
        if (y > maxY) return false;
        if (clay.has(`${x},${y}`) || rest.has(`${x},${y}`)) return true;
        if (water.has(`${x},${y}`)) return false;

        water.add(`${x},${y}`);

        if (!flow(x, y + 1)) return false;

        let left = flow(x - 1, y);
        let right = flow(x + 1, y);

        if (left && right) {
            rest.add(`${x},${y}`);
            let lx = x - 1;
            while (water.has(`${lx},${y}`) && !clay.has(`${lx},${y}`)) {
                rest.add(`${lx},${y}`);
                lx--;
            }
            let rx = x + 1;
            while (water.has(`${rx},${y}`) && !clay.has(`${rx},${y}`)) {
                rest.add(`${rx},${y}`);
                rx++;
            }
        }

        return left || right;
    }

    flow(500, 0);

    return { water, rest };
}

function solve(input) {
    const { clay, minX, maxX, minY, maxY } = parseInput(input);
    const { water, rest } = simulateWater(clay, minX, maxX, minY, maxY);

    const part1 = [...water].filter(coord => {
        const [, y] = coord.split(',').map(Number);
        return y >= minY && y <= maxY;
    }).length;

    const part2 = rest.size;

    return { part1, part2 };
}

const input = fs.readFileSync('input.txt', 'utf8');
const { part1, part2 } = solve(input);
console.log('Part 1:', part1);
console.log('Part 2:', part2);
