const fs = require('fs');

class Coordinate {
    constructor(q, r) {
        this.q = q;
        this.r = r;
    }
}

const directions = {
    "e": { q: 1, r: 0 },
    "se": { q: 0, r: 1 },
    "sw": { q: -1, r: 1 },
    "w": { q: -1, r: 0 },
    "nw": { q: 0, r: -1 },
    "ne": { q: 1, r: -1 },
};

function getNeighbors(tile) {
    const neighbors = [];
    for (const dir in directions) {
        const { q, r } = directions[dir];
        neighbors.push(new Coordinate(tile.q + q, tile.r + r));
    }
    return neighbors;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const blackTiles = new Map();
    const lines = data.trim().split('\n');

    lines.forEach((line) => {
        const coord = new Coordinate(0, 0);
        for (let i = 0; i < line.length; i++) {
            let dir = '';
            switch (line[i]) {
                case 'e':
                case 'w':
                    dir = line[i];
                    break;
                case 'n':
                case 's':
                    dir = line.slice(i, i + 2);
                    i++;
                    break;
            }
            const move = directions[dir];
            coord.q += move.q;
            coord.r += move.r;
        }
        blackTiles.set(JSON.stringify(coord), !blackTiles.get(JSON.stringify(coord)));
    });

    for (let day = 0; day < 100; day++) {
        const tilesToCheck = new Map();
        blackTiles.forEach((value, tile) => {
            if (value) {
                tilesToCheck.set(tile, true);
                getNeighbors(JSON.parse(tile)).forEach((neighbor) => {
                    tilesToCheck.set(JSON.stringify(neighbor), true);
                });
            }
        });

        const newBlackTiles = new Map();
        tilesToCheck.forEach((_, tile) => {
            const blackNeighborCount = getNeighbors(JSON.parse(tile)).reduce((count, neighbor) => {
                return count + (blackTiles.get(JSON.stringify(neighbor)) ? 1 : 0);
            }, 0);

            if (blackTiles.get(tile) && (blackNeighborCount === 1 || blackNeighborCount === 2)) {
                newBlackTiles.set(tile, true);
            } else if (!blackTiles.get(tile) && blackNeighborCount === 2) {
                newBlackTiles.set(tile, true);
            }
        });

        blackTiles.clear();
        newBlackTiles.forEach((_, tile) => {
            blackTiles.set(tile, true);
        });
    }

    console.log(blackTiles.size);
});