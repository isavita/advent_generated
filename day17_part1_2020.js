const fs = require('fs');

class Coordinate {
    constructor(x, y, z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map(line => line.trim());

const activeCubes = new Map();

input.forEach((line, y) => {
    line.split('').forEach((char, x) => {
        if (char === '#') {
            activeCubes.set(JSON.stringify(new Coordinate(x, y, 0)), true);
        }
    });
});

for (let cycle = 0; cycle < 6; cycle++) {
    simulateCycle(activeCubes);
}

console.log(activeCubes.size);

function simulateCycle(activeCubes) {
    const newActiveCubes = new Map();
    const neighborCounts = new Map();

    activeCubes.forEach((_, coordStr) => {
        const coord = JSON.parse(coordStr);
        for (let dz = -1; dz <= 1; dz++) {
            for (let dy = -1; dy <= 1; dy++) {
                for (let dx = -1; dx <= 1; dx++) {
                    if (dz === 0 && dy === 0 && dx === 0) {
                        continue;
                    }
                    const neighbor = new Coordinate(coord.x + dx, coord.y + dy, coord.z + dz);
                    const neighborStr = JSON.stringify(neighbor);
                    neighborCounts.set(neighborStr, (neighborCounts.get(neighborStr) || 0) + 1);
                }
            }
        }
    });

    neighborCounts.forEach((count, neighborStr) => {
        const coord = JSON.parse(neighborStr);
        if (count === 3 || (count === 2 && activeCubes.has(neighborStr))) {
            newActiveCubes.set(neighborStr, true);
        }
    });

    activeCubes.clear();
    newActiveCubes.forEach((_, coordStr) => {
        activeCubes.set(coordStr, true);
    });
}