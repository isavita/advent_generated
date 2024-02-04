
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const activeCubes = new Map();

input.forEach((line, y) => {
    line.split('').forEach((char, x) => {
        if (char === '#') {
            activeCubes.set(`${x},${y},0,0`, true);
        }
    });
});

for (let cycle = 0; cycle < 6; cycle++) {
    simulateCycle4D(activeCubes);
}

console.log(activeCubes.size);

function simulateCycle4D(activeCubes) {
    const newActiveCubes = new Map();
    const neighborCounts = new Map();

    activeCubes.forEach((_, coord) => {
        const [x, y, z, w] = coord.split(',').map(Number);

        for (let dw = -1; dw <= 1; dw++) {
            for (let dz = -1; dz <= 1; dz++) {
                for (let dy = -1; dy <= 1; dy++) {
                    for (let dx = -1; dx <= 1; dx++) {
                        if (dw === 0 && dz === 0 && dy === 0 && dx === 0) {
                            continue;
                        }
                        const neighbor = `${x + dx},${y + dy},${z + dz},${w + dw}`;
                        neighborCounts.set(neighbor, (neighborCounts.get(neighbor) || 0) + 1);
                    }
                }
            }
        }
    });

    neighborCounts.forEach((count, coord) => {
        if (count === 3 || (count === 2 && activeCubes.has(coord))) {
            newActiveCubes.set(coord, true);
        }
    });

    activeCubes.clear();
    newActiveCubes.forEach((_, coord) => {
        activeCubes.set(coord, true);
    });
}
