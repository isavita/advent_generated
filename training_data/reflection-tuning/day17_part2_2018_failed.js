const fs = require('fs');

function simulateWater(clay) {
    const water = new Set();
    const settledWater = new Set();
    let minY = Math.min(...Array.from(clay).map(([,y]) => y));
    let maxY = Math.max(...Array.from(clay).map(([,y]) => y));

    function flow(x, y) {
        if (y > maxY) return false;
        if (clay.has(`${x},${y}`)) return true;
        if (settledWater.has(`${x},${y}`)) return true;
        if (water.has(`${x},${y}`)) return false;

        water.add(`${x},${y}`);

        if (!flow(x, y + 1)) return false;

        let left = flow(x - 1, y);
        let right = flow(x + 1, y);

        if (left && right) {
            settledWater.add(`${x},${y}`);
            return true;
        }

        return false;
    }

    flow(500, 0);

    return {
        reachable: water.size - [...water].filter(pos => parseInt(pos.split(',')[1]) < minY).length,
        settled: settledWater.size
    };
}

function parseInput(input) {
    const clay = new Set();
    input.split('\n').forEach(line => {
        let [a, b] = line.split(', ');
        let [axis1, value1] = a.split('=');
        let [axis2, range] = b.split('=');
        let [start, end] = range.split('..').map(Number);
        
        for (let i = start; i <= end; i++) {
            if (axis1 === 'x') {
                clay.add(`${value1},${i}`);
            } else {
                clay.add(`${i},${value1}`);
            }
        }
    });
    return clay;
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
const clay = parseInput(input);
const result = simulateWater(clay);

console.log('Part 1:', result.reachable);
console.log('Part 2:', result.settled);
