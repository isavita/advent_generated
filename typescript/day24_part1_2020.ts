import * as fs from 'fs';

const directions: { [key: string]: [number, number] } = {
    e: [1, 0],
    se: [0, 1],
    sw: [-1, 1],
    w: [-1, 0],
    nw: [0, -1],
    ne: [1, -1]
};

const flipTiles = (input: string[]): number => {
    const tileMap = new Map<string, boolean>();

    for (const line of input) {
        let x = 0, y = 0;
        let i = 0;

        while (i < line.length) {
            const dir = line[i] === 's' || line[i] === 'n' ? line.substr(i, 2) : line[i];
            const [dx, dy] = directions[dir];
            x += dx;
            y += dy;
            i += dir.length;
        }

        const key = `${x},${y}`;
        tileMap.set(key, !tileMap.get(key)); // Flip the tile
    }

    return Array.from(tileMap.values()).filter(isBlack => isBlack).length;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const blackTileCount = flipTiles(input);
    console.log(blackTileCount);
};

main();