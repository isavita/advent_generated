
import * as fs from 'fs';

type Point = { x: number; y: number };
type State = { pos: Point; step: number };
type Grid = Map<string, string>;

const neighbors4: Point[] = [
    { x: 0, y: 1 },
    { x: 0, y: -1 },
    { x: 1, y: 0 },
    { x: -1, y: 0 },
];

function pointToString(p: Point): string {
    return `${p.x},${p.y}`;
}

function stringToPoint(s: string): Point {
    const [x, y] = s.split(',').map(Number);
    return { x, y };
}

function readAll(path: string): string {
    return fs.readFileSync(path, 'utf-8').trim();
}

function getKeys(grid: Grid): Point[] {
    return Array.from(grid.keys()).map(stringToPoint);
}

function bounds(points: Point[]): { min: Point; max: Point } {
    let minX = Infinity, minY = Infinity, maxX = -Infinity, maxY = -Infinity;
    for (const p of points) {
        minX = Math.min(minX, p.x);
        minY = Math.min(minY, p.y);
        maxX = Math.max(maxX, p.x);
        maxY = Math.max(maxY, p.y);
    }
    return { min: { x: minX, y: minY }, max: { x: maxX + 1, y: maxY + 1 } };
}

function add(p1: Point, p2: Point): Point {
    return { x: p1.x + p2.x, y: p1.y + p2.y };
}

function sub(p1: Point, p2: Point): Point {
    return { x: p1.x - p2.x, y: p1.y - p2.y };
}

function mod(p: Point, bounds: { min: Point; max: Point }): Point {
    const width = bounds.max.x - bounds.min.x;
    const height = bounds.max.y - bounds.min.y;
    return {
        x: ((p.x - bounds.min.x) % width + width) % width + bounds.min.x,
        y: ((p.y - bounds.min.y) % height + height) % height + bounds.min.y,
    };
}

function isInBounds(p: Point, bounds: { min: Point; max: Point }): boolean {
    return p.x >= bounds.min.x && p.x < bounds.max.x && p.y >= bounds.min.y && p.y < bounds.max.y;
}

const dirFromByte: { [key: string]: Point } = {
    '^': { x: 0, y: -1 },
    '>': { x: 1, y: 0 },
    'v': { x: 0, y: 1 },
    '<': { x: -1, y: 0 },
};

function steps(grid: Grid, bounds: { min: Point; max: Point }, start: Point, end: Point, initialStep: number): number {
    const q: State[] = [{ pos: start, step: initialStep }];
    const seen = new Set<string>();

    while (q.length > 0) {
        const curr = q.shift()!;
        if (curr.pos.x === end.x && curr.pos.y === end.y) {
            return curr.step;
        }

        for (const n of [...neighbors4, { x: 0, y: 0 }]) {
            const nextPos = add(curr.pos, n);
            const nextState: State = { pos: nextPos, step: curr.step + 1 };
            const nextStateKey = `${pointToString(nextState.pos)},${nextState.step}`;

            if (seen.has(nextStateKey)) {
                continue;
            }
            if (!isInBounds(nextState.pos, bounds)) {
                continue;
            }
            if (grid.get(pointToString(nextState.pos)) === '#') {
                continue;
            }

            if (nextState.pos.y > bounds.min.y && nextState.pos.y < bounds.max.y - 1) {
                let blocked = false;
                for (const bliz of ['^', '>', 'v', '<']) {
                    const prev = mod(sub(nextState.pos, {x: dirFromByte[bliz].x * nextState.step, y: dirFromByte[bliz].y * nextState.step}), {min: {x: bounds.min.x + 1, y: bounds.min.y + 1}, max: {x: bounds.max.x - 1, y: bounds.max.y - 1}});
                    if (grid.get(pointToString(prev)) === bliz) {
                        blocked = true;
                        break;
                    }
                }
                if (blocked) {
                    continue;
                }
            }
            q.push(nextState);
            seen.add(nextStateKey);
        }
    }
    return -1;
}

function main() {
    const grid: Grid = new Map();
    const input = readAll('input.txt');
    const lines = input.split('\n');

    for (let y = 0; y < lines.length; y++) {
        const line = lines[y];
        for (let x = 0; x < line.length; x++) {
            if (line[x] !== '.') {
                grid.set(pointToString({ x, y }), line[x]);
            }
        }
    }

    const boundsData = bounds(getKeys(grid));
    const entrance: Point = { x: 1, y: 0 };
    const exit: Point = { x: boundsData.max.x - 2, y: boundsData.max.y - 1 };

    const firstCrossing = steps(grid, boundsData, entrance, exit, 0);
    const secondCrossing = steps(grid, boundsData, exit, entrance, firstCrossing);
    const thirdCrossing = steps(grid, boundsData, entrance, exit, secondCrossing);

    console.log(thirdCrossing);
}

main();
