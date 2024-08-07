import * as fs from 'fs';

const Open = '.';
const Trees = '|';
const Lumberyard = '#';
const Size = 50;

function main() {
    let grid = readInput("input.txt");
    const seenStates = new Map<string, number>();
    let cycleStart = 0, cycleLength = 0;

    for (let minute = 0; ; minute++) {
        const state = gridToString(grid);
        if (seenStates.has(state)) {
            cycleStart = seenStates.get(state)!;
            cycleLength = minute - cycleStart;
            break;
        }
        seenStates.set(state, minute);
        grid = transform(grid);
    }

    const remainingMinutes = (1000000000 - cycleStart) % cycleLength;
    for (let i = 0; i < remainingMinutes; i++) {
        grid = transform(grid);
    }

    const [wooded, lumberyards] = countResources(grid);
    console.log(wooded * lumberyards);
}

function readInput(filename: string): string[][] {
    const data = fs.readFileSync(filename, 'utf-8');
    return data.split('\n').map(line => line.split(''));
}

function transform(grid: string[][]): string[][] {
    const newGrid = grid.map((row, i) => row.map((_, j) => nextAcreState(grid, i, j)));
    return newGrid;
}

function nextAcreState(grid: string[][], i: number, j: number): string {
    switch (grid[i][j]) {
        case Open:
            if (countAdjacent(grid, i, j, Trees) >= 3) {
                return Trees;
            }
            break;
        case Trees:
            if (countAdjacent(grid, i, j, Lumberyard) >= 3) {
                return Lumberyard;
            }
            break;
        case Lumberyard:
            if (countAdjacent(grid, i, j, Lumberyard) >= 1 && countAdjacent(grid, i, j, Trees) >= 1) {
                return Lumberyard;
            }
            return Open;
    }
    return grid[i][j];
}

function countAdjacent(grid: string[][], i: number, j: number, acreType: string): number {
    let count = 0;
    for (let x = -1; x <= 1; x++) {
        for (let y = -1; y <= 1; y++) {
            if (x === 0 && y === 0) continue;
            if (i + x >= 0 && i + x < grid.length && j + y >= 0 && j + y < grid[i].length && grid[i + x][j + y] === acreType) {
                count++;
            }
        }
    }
    return count;
}

function countResources(grid: string[][]): [number, number] {
    let wooded = 0, lumberyards = 0;
    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[i].length; j++) {
            if (grid[i][j] === Trees) wooded++;
            if (grid[i][j] === Lumberyard) lumberyards++;
        }
    }
    return [wooded, lumberyards];
}

function gridToString(grid: string[][]): string {
    return grid.map(row => row.join('')).join('\n');
}

main();