import * as fs from 'fs';

type Grid = boolean[][];

const filePath = 'input.txt';
const gridSize = 100;
const steps = 100;

function readInput(filePath: string): Grid {
    const data = fs.readFileSync(filePath, 'utf-8');
    const lines = data.split('\n').filter(line => line.trim() !== '');
    const grid: Grid = lines.map(line => line.split('').map(char => char === '#'));
    return grid;
}

function countNeighbors(grid: Grid, x: number, y: number): number {
    let count = 0;
    for (let i = -1; i <= 1; i++) {
        for (let j = -1; j <= 1; j++) {
            if (i === 0 && j === 0) continue;
            const nx = x + i;
            const ny = y + j;
            if (nx >= 0 && nx < gridSize && ny >= 0 && ny < gridSize && grid[nx][ny]) {
                count++;
            }
        }
    }
    return count;
}

function nextState(grid: Grid): Grid {
    const newGrid: Grid = Array.from({ length: gridSize }, () => new Array(gridSize).fill(false));
    for (let x = 0; x < gridSize; x++) {
        for (let y = 0; y < gridSize; y++) {
            const neighbors = countNeighbors(grid, x, y);
            if (grid[x][y]) {
                newGrid[x][y] = neighbors === 2 || neighbors === 3;
            } else {
                newGrid[x][y] = neighbors === 3;
            }
        }
    }
    return newGrid;
}

function countLightsOn(grid: Grid): number {
    return grid.reduce((count, row) => count + row.filter(light => light).length, 0);
}

function main() {
    const initialGrid = readInput(filePath);
    let currentGrid = initialGrid;

    for (let step = 0; step < steps; step++) {
        currentGrid = nextState(currentGrid);
    }

    const lightsOn = countLightsOn(currentGrid);
    console.log(`Lights on after ${steps} steps: ${lightsOn}`);
}

main();