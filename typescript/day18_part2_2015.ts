import * as fs from 'fs';

// Function to read the input file and return the initial grid
function readInput(filePath: string): string[][] {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    return data.map(line => line.split(''));
}

// Function to count the number of lights that are on
function countLightsOn(grid: string[][]): number {
    return grid.flat().filter(light => light === '#').length;
}

// Function to get the number of neighbors that are on
function countNeighborsOn(grid: string[][], x: number, y: number): number {
    const directions = [
        [-1, -1], [-1, 0], [-1, 1],
        [0, -1],          [0, 1],
        [1, -1], [1, 0], [1, 1]
    ];
    let count = 0;
    for (const [dx, dy] of directions) {
        const nx = x + dx;
        const ny = y + dy;
        if (nx >= 0 && nx < grid.length && ny >= 0 && ny < grid[0].length && grid[nx][ny] === '#') {
            count++;
        }
    }
    return count;
}

// Function to update the grid based on the game of life rules
function updateGrid(grid: string[][]): string[][] {
    const newGrid = grid.map(row => row.slice());
    for (let i = 0; i < grid.length; i++) {
        for (let j = 0; j < grid[0].length; j++) {
            const neighborsOn = countNeighborsOn(grid, i, j);
            if (grid[i][j] === '#') {
                if (neighborsOn === 2 || neighborsOn === 3) {
                    newGrid[i][j] = '#';
                } else {
                    newGrid[i][j] = '.';
                }
            } else {
                if (neighborsOn === 3) {
                    newGrid[i][j] = '#';
                } else {
                    newGrid[i][j] = '.';
                }
            }
        }
    }
    return newGrid;
}

// Function to run the game of life for a given number of steps
function runGameOfLife(grid: string[][], steps: number, cornersStuckOn: boolean = false): number {
    for (let step = 0; step < steps; step++) {
        grid = updateGrid(grid);
        if (cornersStuckOn) {
            grid[0][0] = grid[0][grid[0].length - 1] = '#';
            grid[grid.length - 1][0] = grid[grid.length - 1][grid[0].length - 1] = '#';
        }
    }
    return countLightsOn(grid);
}

// Main function
function main() {
    const grid = readInput('input.txt');

    // Part 1
    const part1Grid = JSON.parse(JSON.stringify(grid));
    const lightsOnPart1 = runGameOfLife(part1Grid, 100);
    console.log(`Part 1: ${lightsOnPart1} lights are on after 100 steps.`);

    // Part 2
    const part2Grid = JSON.parse(JSON.stringify(grid));
    part2Grid[0][0] = part2Grid[0][part2Grid[0].length - 1] = '#';
    part2Grid[part2Grid.length - 1][0] = part2Grid[part2Grid.length - 1][part2Grid[0].length - 1] = '#';
    const lightsOnPart2 = runGameOfLife(part2Grid, 100, true);
    console.log(`Part 2: ${lightsOnPart2} lights are on after 100 steps with corners stuck on.`);
}

main();