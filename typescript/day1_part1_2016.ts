import * as fs from 'fs';

// Function to read input from file
function readInput(filePath: string): string[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split(', ');
}

// Function to calculate the Manhattan distance
function calculateDistance(instructions: string[]): number {
    let x = 0;
    let y = 0;
    let direction = 0; // 0: North, 1: East, 2: South, 3: West

    for (const instruction of instructions) {
        const turn = instruction[0];
        const distance = parseInt(instruction.slice(1), 10);

        if (turn === 'R') {
            direction = (direction + 1) % 4;
        } else if (turn === 'L') {
            direction = (direction + 3) % 4;
        }

        if (direction === 0) {
            y += distance;
        } else if (direction === 1) {
            x += distance;
        } else if (direction === 2) {
            y -= distance;
        } else if (direction === 3) {
            x -= distance;
        }
    }

    return Math.abs(x) + Math.abs(y);
}

// Main function
function main() {
    const instructions = readInput('input.txt');
    const distance = calculateDistance(instructions);
    console.log(`The shortest path to the destination is ${distance} blocks away.`);
}

// Run the main function
main();