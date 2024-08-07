import * as fs from 'fs/promises';

type Coord = { x: number, y: number };

class Grid {
    width: number;
    height: number;
    data: Map<string, string>;

    constructor(width: number, height: number, data: Map<string, string>) {
        this.width = width;
        this.height = height;
        this.data = data;
    }

    toString(): string {
        let res = "";
        for (let y = 0; y < this.height; y++) {
            for (let x = 0; x < this.width; x++) {
                const key = `${x},${y}`;
                res += this.data.has(key) ? this.data.get(key) : '.';
            }
            res += "\n";
        }
        return res;
    }
}

const North = { x: 0, y: -1 };
const West = { x: -1, y: 0 };
const South = { x: 0, y: 1 };
const East = { x: 1, y: 0 };

const Empty = '.';
const Rock = '#';
const Start = 'S';

function add(c1: Coord, c2: Coord): Coord {
    return { x: c1.x + c2.x, y: c1.y + c2.y };
}

function multiplyByScalar(c: Coord, s: number): Coord {
    return { x: c.x * s, y: c.y * s };
}

function isInBounds(grid: Grid, coord: Coord): boolean {
    return 0 <= coord.x && coord.x < grid.width && 0 <= coord.y && coord.y < grid.height;
}

function parseInput(input: string[]): Grid {
    const data = new Map<string, string>();
    const width = input[0].length;
    const height = input.length;

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            const char = input[y][x];
            if (char !== Empty) {
                data.set(`${x},${y}`, char);
            }
        }
    }

    return new Grid(width, height, data);
}

function findStart(grid: Grid): Coord {
    for (const [key, char] of grid.data) {
        if (char === Start) {
            const [x, y] = key.split(',').map(Number);
            return { x, y };
        }
    }
    throw new Error("No start found.");
}

function neighbors4(grid: Grid, coord: Coord): Coord[] {
    const neighbors = [add(coord, North), add(coord, South), add(coord, East), add(coord, West)];
    return neighbors.filter(neighbor => isInBounds(grid, neighbor) && grid.data.get(`${neighbor.x},${neighbor.y}`) !== Rock);
}

function breadthFirstSearch(grid: Grid, start: Coord, neighborFunc: (grid: Grid, coord: Coord) => Coord[]): Map<string, number> {
    const frontier: Coord[] = [start];
    const reached = new Set<string>([`${start.x},${start.y}`]);
    const distances = new Map<string, number>([[`${start.x},${start.y}`, 0]]);

    while (frontier.length > 0) {
        const current = frontier.shift()!;
        for (const next of neighborFunc(grid, current)) {
            const nextKey = `${next.x},${next.y}`;
            if (!reached.has(nextKey)) {
                frontier.push(next);
                reached.add(nextKey);
                distances.set(nextKey, distances.get(`${current.x},${current.y}`)! + 1);
            }
        }
    }

    return distances;
}

function solve(input: string[], numSteps: number): number {
    const grid = parseInput(input);
    const start = findStart(grid);
    const distances = breadthFirstSearch(grid, start, neighbors4);

    let cnt = 0;
    for (const dist of distances.values()) {
        if (dist <= numSteps && dist % 2 === 0) {
            cnt++;
        }
    }
    return cnt;
}

async function readFile(fileName: string): Promise<string[]> {
    const data = await fs.readFile(fileName, 'utf-8');
    return data.trim().split('\n');
}

(async () => {
    const input = await readFile("input.txt");
    console.log(solve(input, 64));
})();