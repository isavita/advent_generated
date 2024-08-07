import * as fs from 'fs';
import * as path from 'path';

// Function to read input from a file
function readInput(filePath: string): string {
    const fileContents = fs.readFileSync(filePath, 'utf-8');
    return fileContents.trim();
}

// Function to determine if a coordinate is a wall or open space
function isWall(x: number, y: number, favoriteNumber: number): boolean {
    const sum = x*x + 3*x + 2*x*y + y + y*y + favoriteNumber;
    const binaryRepresentation = sum.toString(2);
    const numberOfOnes = binaryRepresentation.split('').filter(bit => bit === '1').length;
    return numberOfOnes % 2 === 1;
}

// Function to find the shortest path using BFS
function shortestPath(favoriteNumber: number, targetX: number, targetY: number): number {
    const queue: [number, number, number][] = [[1, 1, 0]]; // [x, y, steps]
    const visited = new Set<string>();
    visited.add(`1,1`);

    while (queue.length > 0) {
        const [x, y, steps] = queue.shift()!;

        if (x === targetX && y === targetY) {
            return steps;
        }

        const directions = [
            [0, 1], // Up
            [0, -1], // Down
            [1, 0], // Right
            [-1, 0] // Left
        ];

        for (const [dx, dy] of directions) {
            const newX = x + dx;
            const newY = y + dy;

            if (newX >= 0 && newY >= 0 && !isWall(newX, newY, favoriteNumber) && !visited.has(`${newX},${newY}`)) {
                visited.add(`${newX},${newY}`);
                queue.push([newX, newY, steps + 1]);
            }
        }
    }

    return -1; // If no path is found
}

// Main function
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const favoriteNumber = parseInt(readInput(filePath), 10);
    const targetX = 31;
    const targetY = 39;

    const steps = shortestPath(favoriteNumber, targetX, targetY);
    console.log(`The fewest number of steps required to reach (${targetX},${targetY}) is ${steps}.`);
}

main();