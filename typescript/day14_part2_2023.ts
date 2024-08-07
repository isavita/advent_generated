type Coord = {
    x: number;
    y: number;
};

const addCoord = (c1: Coord, c2: Coord): Coord => ({ x: c1.x + c2.x, y: c1.y + c2.y });

type Grid = {
    width: number;
    height: number;
    data: Map<string, string>;
};

const isInBounds = (coord: Coord, grid: Grid): boolean => coord.x >= 0 && coord.x < grid.width && coord.y >= 0 && coord.y < grid.height;

const Empty = '.';
const CubicRock = '#';
const RoundRock = 'O';

const North: Coord = { x: 0, y: -1 };
const West: Coord = { x: -1, y: 0 };
const South: Coord = { x: 0, y: 1 };
const East: Coord = { x: 1, y: 0 };

const buildGrid = (input: string[]): Grid => {
    const grid: Grid = {
        width: input[0].length,
        height: input.length,
        data: new Map(),
    };

    for (let y = 0; y < input.length; y++) {
        for (let x = 0; x < input[y].length; x++) {
            if (input[y][x] !== Empty) {
                grid.data.set(`${x},${y}`, input[y][x]);
            }
        }
    }

    return grid;
};

const toString = (grid: Grid): string => {
    let result = '';

    for (let y = 0; y < grid.height; y++) {
        for (let x = 0; x < grid.width; x++) {
            const coord = `${x},${y}`;
            result += grid.data.has(coord) ? grid.data.get(coord) : Empty;
        }
        result += '\n';
    }

    return result;
};

const shiftSingleRock = (grid: Grid, coord: Coord, dir: Coord) => {
    if (grid.data.get(`${coord.x},${coord.y}`) === RoundRock) {
        let current = coord;
        let before = addCoord(coord, dir);

        while (!grid.data.has(`${before.x},${before.y}`) && isInBounds(before, grid)) {
            grid.data.set(`${before.x},${before.y}`, RoundRock);
            grid.data.delete(`${current.x},${current.y}`);

            current = before;
            before = addCoord(before, dir);
        }
    }
};

const shiftRocks = (grid: Grid, dir: Coord) => {
    if (dir === North || dir === West) {
        for (let x = 0; x < grid.width; x++) {
            for (let y = 0; y < grid.height; y++) {
                shiftSingleRock(grid, { x, y }, dir);
            }
        }
    } else {
        for (let x = grid.width - 1; x >= 0; x--) {
            for (let y = grid.height - 1; y >= 0; y--) {
                shiftSingleRock(grid, { x, y }, dir);
            }
        }
    }
};

const cycleRocks = (grid: Grid) => {
    shiftRocks(grid, North);
    shiftRocks(grid, West);
    shiftRocks(grid, South);
    shiftRocks(grid, East);
};

const calculateGridKey = (grid: Grid): number => {
    let key = 0;

    for (let x = 0; x < grid.width; x++) {
        for (let y = 0; y < grid.height; y++) {
            const coord = `${x},${y}`;
            if (grid.data.get(coord) === RoundRock) {
                key += x + y * grid.width;
            }
        }
    }

    return key;
};

const calculateLoad = (grid: Grid): number => {
    let load = 0;

    for (let x = 0; x < grid.width; x++) {
        for (let y = 0; y < grid.height; y++) {
            const coord = `${x},${y}`;
            if (grid.data.get(coord) === RoundRock) {
                load += grid.height - y;
            }
        }
    }

    return load;
};

const solve = (input: string[]): number => {
    const numCycles = 1000000000;
    const grid = buildGrid(input);
    const cache = new Map<number, number>();

    for (let i = 0; i < numCycles; i++) {
        const gridKey = calculateGridKey(grid);
        if (cache.has(gridKey)) {
            const iStartCycle = cache.get(gridKey)!;
            const remainingCycles = (numCycles - iStartCycle) % (i - iStartCycle);
            for (let j = 0; j < remainingCycles; j++) {
                cycleRocks(grid);
            }
            return calculateLoad(grid);
        }
        cache.set(gridKey, i);
        cycleRocks(grid);
    }

    return calculateLoad(grid);
};

const readFile = (fileName: string): string[] => {
    const fs = require('fs');
    const file = fs.readFileSync(fileName, 'utf-8');
    return file.trim().split('\n');
};

const input = readFile('input.txt');
console.log(solve(input));