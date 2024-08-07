import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const grid = data.map(line => line.split(''));

console.log(findSafeStep(grid));

function findSafeStep(grid: string[][]): number {
    let step = 0;
    while (true) {
        const eastMoved = moveEast(grid);
        const southMoved = moveSouth(grid);
        step++;
        if (!eastMoved && !southMoved) {
            break;
        }
    }
    return step;
}

function moveEast(grid: string[][]): boolean {
    let moved = false;
    const height = grid.length;
    const width = grid[0].length;
    const oldPositions = grid.map(row => row.map(() => ' '));

    for (let y = 0; y < height; y++) {
        for (let x = 0; x < width; x++) {
            if (grid[y][x] === '>') {
                const nextX = (x + 1) % width;
                if (grid[y][nextX] === '.') {
                    oldPositions[y][x] = '.';
                    grid[y][nextX] = '>';
                    x++;
                    moved = true;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions);
    return moved;
}

function moveSouth(grid: string[][]): boolean {
    let moved = false;
    const height = grid.length;
    const width = grid[0].length;
    const oldPositions = grid.map(row => row.map(() => ' '));

    for (let x = 0; x < width; x++) {
        for (let y = 0; y < height; y++) {
            if (grid[y][x] === 'v') {
                const nextY = (y + 1) % height;
                if (grid[nextY][x] === '.') {
                    oldPositions[y][x] = '.';
                    grid[nextY][x] = 'v';
                    y++;
                    moved = true;
                }
            }
        }
    }
    freeEmptyPositions(grid, oldPositions);
    return moved;
}

function freeEmptyPositions(grid: string[][], oldPositions: string[][]) {
    for (let y = 0; y < grid.length; y++) {
        for (let x = 0; x < grid[0].length; x++) {
            if (oldPositions[y][x] === '.') {
                grid[y][x] = '.';
            }
        }
    }
}