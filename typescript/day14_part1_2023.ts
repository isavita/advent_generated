import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const rows = input.length;
const cols = input[0].length;

const calculateLoad = (grid: string[][]): number => {
    let totalLoad = 0;
    for (let r = 0; r < rows; r++) {
        for (let c = 0; c < cols; c++) {
            if (grid[r][c] === 'O') {
                totalLoad += (rows - r);
            }
        }
    }
    return totalLoad;
};

const tiltNorth = (grid: string[][]): string[][] => {
    const newGrid = Array.from({ length: rows }, () => Array(cols).fill('.'));
    for (let c = 0; c < cols; c++) {
        let pointer = 0;
        for (let r = 0; r < rows; r++) {
            if (grid[r][c] === 'O') {
                newGrid[pointer++][c] = 'O';
            } else if (grid[r][c] === '#') {
                newGrid[r][c] = '#';
                pointer = Math.max(pointer, r + 1); // Move pointer below the cube-shaped rock
            }
        }
    }
    return newGrid;
};

const main = () => {
    const grid = input.map(line => line.split(''));
    const tiltedGrid = tiltNorth(grid);
    const totalLoad = calculateLoad(tiltedGrid);
    console.log(totalLoad);
};

main();