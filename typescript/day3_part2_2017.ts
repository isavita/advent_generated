import * as fs from 'fs';
import * as path from 'path';

// Function to calculate the Manhattan distance from the center
function getManhattanDistance(square: number): number {
    let layer = 0;
    let layerSize = 1;
    let maxValue = 1;

    while (maxValue < square) {
        layer++;
        layerSize = layer * 2;
        maxValue = layerSize * layerSize;
    }

    const sideLength = layer * 2 + 1;
    const maxSideValue = maxValue;
    const minSideValue = maxSideValue - (sideLength - 1);

    const sideOffset = (square - minSideValue) % (sideLength - 1);
    const distanceFromCenter = layer + Math.abs(sideOffset - (sideLength - 1) / 2);

    return distanceFromCenter;
}

// Function to find the first value larger than the input in the spiral sum pattern
function getFirstLargerValue(square: number): number {
    const directions = [
        [0, 1], [-1, 0], [0, -1], [1, 0]
    ];

    const grid: number[][] = [];
    let x = 0, y = 0;
    let value = 1;
    let directionIndex = 0;
    let steps = 1;
    let stepCount = 0;

    grid[y] = [];
    grid[y][x] = value;

    while (value <= square) {
        const [dx, dy] = directions[directionIndex];

        for (let i = 0; i < steps; i++) {
            x += dx;
            y += dy;

            if (!grid[y]) grid[y] = [];

            value = 0;
            for (let j = -1; j <= 1; j++) {
                for (let k = -1; k <= 1; k++) {
                    if (j === 0 && k === 0) continue;
                    if (grid[y + j] && grid[y + j][x + k]) {
                        value += grid[y + j][x + k];
                    }
                }
            }

            grid[y][x] = value;
            if (value > square) return value;
        }

        directionIndex = (directionIndex + 1) % 4;
        stepCount++;

        if (stepCount === 2) {
            steps++;
            stepCount = 0;
        }
    }

    return value;
}

// Main function to read input and print results
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(filePath, 'utf-8').trim();
    const square = parseInt(input, 10);

    const distance = getManhattanDistance(square);
    const firstLargerValue = getFirstLargerValue(square);

    console.log(`Part 1: ${distance}`);
    console.log(`Part 2: ${firstLargerValue}`);
}

main();