import * as fs from 'fs';

type Tile = { x: number; y: number };

const directions: { [key: string]: Tile } = {
    e: { x: 1, y: 0 },
    se: { x: 0, y: 1 },
    sw: { x: -1, y: 1 },
    w: { x: -1, y: 0 },
    nw: { x: 0, y: -1 },
    ne: { x: 1, y: -1 }
};

const flipTiles = (instructions: string[]): Set<string> => {
    const blackTiles = new Set<string>();

    for (const instruction of instructions) {
        let pos: Tile = { x: 0, y: 0 };
        let i = 0;

        while (i < instruction.length) {
            if (instruction[i] === 's' || instruction[i] === 'n') {
                const dir = instruction[i] + instruction[i + 1];
                pos.x += directions[dir].x;
                pos.y += directions[dir].y;
                i += 2;
            } else {
                pos.x += directions[instruction[i]].x;
                pos.y += directions[instruction[i]].y;
                i += 1;
            }
        }

        const key = `${pos.x},${pos.y}`;
        if (blackTiles.has(key)) {
            blackTiles.delete(key);
        } else {
            blackTiles.add(key);
        }
    }

    return blackTiles;
};

const countAdjacentBlackTiles = (tile: Tile, blackTiles: Set<string>): number => {
    return Object.keys(directions).reduce((count, dir) => {
        const neighbor = { x: tile.x + directions[dir].x, y: tile.y + directions[dir].y };
        return count + (blackTiles.has(`${neighbor.x},${neighbor.y}`) ? 1 : 0);
    }, 0);
};

const simulateDays = (blackTiles: Set<string>, days: number): number => {
    for (let day = 0; day < days; day++) {
        const newBlackTiles = new Set<string>();
        const candidates = new Set<string>();

        blackTiles.forEach(tile => candidates.add(tile));

        blackTiles.forEach(tile => {
            const [x, y] = tile.split(',').map(Number);
            const tileObj: Tile = { x, y };
            for (const dir of Object.keys(directions)) {
                const neighbor = { x: tileObj.x + directions[dir].x, y: tileObj.y + directions[dir].y };
                candidates.add(`${neighbor.x},${neighbor.y}`);
            }
        });

        candidates.forEach(tile => {
            const [x, y] = tile.split(',').map(Number);
            const tileObj: Tile = { x, y };
            const adjacentBlackCount = countAdjacentBlackTiles(tileObj, blackTiles);

            if (blackTiles.has(tile) && (adjacentBlackCount === 0 || adjacentBlackCount > 2)) {
                return; // Tile flips to white, do not add
            }

            if (!blackTiles.has(tile) && adjacentBlackCount === 2) {
                newBlackTiles.add(tile); // Tile flips to black
            }

            if (blackTiles.has(tile)) {
                newBlackTiles.add(tile); // Stay black
            }
        });

        blackTiles = newBlackTiles;
    }
    return blackTiles.size;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const blackTiles = flipTiles(input);
    const result = simulateDays(blackTiles, 100);
    console.log(result);
};

main();