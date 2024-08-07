import * as fs from 'fs';

type Coord = { X: number, Y: number };
type Grid = { Width: number, Height: number, Data: Map<string, string> };
type Beam = { Origin: Coord, Dir: Coord };

const Empty = '.';
const AscendingMirror = '/';
const DescendingMirror = '\\';
const VerticalSplitter = '|';
const HorizontalSplitter = '-';

const North = { X: 0, Y: -1 };
const West = { X: -1, Y: 0 };
const South = { X: 0, Y: 1 };
const East = { X: 1, Y: 0 };

function add(c1: Coord, c2: Coord): Coord {
    return { X: c1.X + c2.X, Y: c1.Y + c2.Y };
}

function rotate90(coord: Coord): Coord {
    return { X: coord.Y, Y: -coord.X };
}

function rotateNeg90(coord: Coord): Coord {
    return { X: -coord.Y, Y: coord.X };
}

function isInBounds(coord: Coord, grid: Grid): boolean {
    return 0 <= coord.X && coord.X < grid.Width && 0 <= coord.Y && coord.Y < grid.Height;
}

function buildGrid(input: string[]): Grid {
    const grid: Grid = {
        Width: input[0].length,
        Height: input.length,
        Data: new Map(),
    };

    for (let y = 0; y < input.length; y++) {
        for (let x = 0; x < input[y].length; x++) {
            if (input[y][x] !== Empty) {
                grid.Data.set(`${x},${y}`, input[y][x]);
            }
        }
    }

    return grid;
}

function nextBeam(grid: Grid, beam: Beam): Beam[] {
    const beams: Beam[] = [];
    const char = grid.Data.get(`${beam.Origin.X},${beam.Origin.Y}`);

    if (!char) {
        return [{ Origin: add(beam.Origin, beam.Dir), Dir: beam.Dir }];
    }

    switch (char) {
        case AscendingMirror:
            const newDir1 = beam.Dir.X === 0 ? rotateNeg90(beam.Dir) : rotate90(beam.Dir);
            beams.push({ Origin: add(beam.Origin, newDir1), Dir: newDir1 });
            break;
        case DescendingMirror:
            const newDir2 = beam.Dir.X === 0 ? rotate90(beam.Dir) : rotateNeg90(beam.Dir);
            beams.push({ Origin: add(beam.Origin, newDir2), Dir: newDir2 });
            break;
        case VerticalSplitter:
            if (beam.Dir.X !== 0) {
                const newDir3 = rotate90(beam.Dir);
                const newDir4 = rotateNeg90(beam.Dir);
                beams.push({ Origin: add(beam.Origin, newDir3), Dir: newDir3 });
                beams.push({ Origin: add(beam.Origin, newDir4), Dir: newDir4 });
            } else {
                beams.push({ Origin: add(beam.Origin, beam.Dir), Dir: beam.Dir });
            }
            break;
        case HorizontalSplitter:
            if (beam.Dir.Y !== 0) {
                const newDir5 = rotate90(beam.Dir);
                const newDir6 = rotateNeg90(beam.Dir);
                beams.push({ Origin: add(beam.Origin, newDir5), Dir: newDir5 });
                beams.push({ Origin: add(beam.Origin, newDir6), Dir: newDir6 });
            } else {
                beams.push({ Origin: add(beam.Origin, beam.Dir), Dir: beam.Dir });
            }
            break;
        default:
            beams.push({ Origin: add(beam.Origin, beam.Dir), Dir: beam.Dir });
    }

    return beams;
}

function calculatePropagation(grid: Grid, start: Beam): Map<string, boolean> {
    const alreadySeen = new Map<string, boolean>();
    const toExplore: Beam[] = [start];

    while (toExplore.length > 0) {
        const beam = toExplore.shift()!;
        const key = `${beam.Origin.X},${beam.Origin.Y},${beam.Dir.X},${beam.Dir.Y}`;

        if (isInBounds(beam.Origin, grid) && !alreadySeen.has(key)) {
            alreadySeen.set(key, true);
            toExplore.push(...nextBeam(grid, beam));
        }
    }

    return alreadySeen;
}

function calculateEnergization(alreadySeen: Map<string, boolean>): Set<string> {
    const alreadyEnergized = new Set<string>();

    for (const key of alreadySeen.keys()) {
        const [x, y] = key.split(',').map(Number);
        alreadyEnergized.add(`${x},${y}`);
    }

    return alreadyEnergized;
}

function getBorder(grid: Grid): Beam[] {
    const border: Beam[] = [];

    for (let x = 0; x < grid.Width; x++) {
        border.push({ Origin: { X: x, Y: 0 }, Dir: South });
        border.push({ Origin: { X: x, Y: grid.Height - 1 }, Dir: North });
    }

    for (let y = 0; y < grid.Height; y++) {
        border.push({ Origin: { X: 0, Y: y }, Dir: East });
        border.push({ Origin: { X: grid.Width - 1, Y: y }, Dir: West });
    }

    return border;
}

function solve(input: string[]): number {
    const grid = buildGrid(input);
    const starts = getBorder(grid);

    let res = 0;
    for (const start of starts) {
        const alreadySeen = calculatePropagation(grid, start);
        const alreadyEnergized = calculateEnergization(alreadySeen);

        const energy = alreadyEnergized.size;
        if (energy > res) {
            res = energy;
        }
    }

    return res;
}

function readFile(fileName: string): string[] {
    return fs.readFileSync(fileName, 'utf-8').trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));