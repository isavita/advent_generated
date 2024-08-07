import * as fs from 'fs';
import * as readline from 'readline';

type Direction = 'n' | 'ne' | 'se' | 's' | 'sw' | 'nw';

interface HexCoordinate {
    q: number;
    r: number;
    s: number;
}

const directions: Record<Direction, HexCoordinate> = {
    n: { q: 0, r: -1, s: 1 },
    ne: { q: 1, r: -1, s: 0 },
    se: { q: 1, r: 0, s: -1 },
    s: { q: 0, r: 1, s: -1 },
    sw: { q: -1, r: 1, s: 0 },
    nw: { q: -1, r: 0, s: 1 }
};

function distance(a: HexCoordinate, b: HexCoordinate): number {
    return (Math.abs(a.q - b.q) + Math.abs(a.r - b.r) + Math.abs(a.s - b.s)) / 2;
}

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let currentPosition: HexCoordinate = { q: 0, r: 0, s: 0 };
    let furthestDistance = 0;

    for await (const line of rl) {
        const steps = line.split(',');
        for (const step of steps) {
            const dir = step.trim() as Direction;
            const move = directions[dir];
            currentPosition.q += move.q;
            currentPosition.r += move.r;
            currentPosition.s += move.s;

            const currentDistance = distance(currentPosition, { q: 0, r: 0, s: 0 });
            if (currentDistance > furthestDistance) {
                furthestDistance = currentDistance;
            }
        }
    }

    console.log(`Fewest steps required: ${distance(currentPosition, { q: 0, r: 0, s: 0 })}`);
    console.log(`Furthest distance: ${furthestDistance}`);
}

processLineByLine();