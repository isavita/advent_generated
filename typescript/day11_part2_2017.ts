import * as fs from 'fs';

type Direction = 'n' | 'ne' | 'se' | 's' | 'sw' | 'nw';

const directionVectors: Record<Direction, [number, number, number]> = {
    n: [0, 1, -1],
    ne: [1, 0, -1],
    se: [1, -1, 0],
    s: [0, -1, 1],
    sw: [-1, 0, 1],
    nw: [-1, 1, 0],
};

function distance(x: number, y: number, z: number): number {
    return (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split(',');
    let x = 0, y = 0, z = 0;
    let maxDistance = 0;

    for (const dir of input) {
        const [dx, dy, dz] = directionVectors[dir as Direction];
        x += dx;
        y += dy;
        z += dz;
        maxDistance = Math.max(maxDistance, distance(x, y, z));
    }

    const finalDistance = distance(x, y, z);

    console.log(`Fewest number of steps required to reach the child process: ${finalDistance}`);
    console.log(`Furthest the child process ever got from the starting position: ${maxDistance}`);
}

main();