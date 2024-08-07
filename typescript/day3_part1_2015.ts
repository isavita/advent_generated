import * as fs from 'fs';

// Function to read the input file and return its content as a string
function readInput(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8');
}

// Function to calculate the number of houses that receive at least one present
function countHousesWithPresents(directions: string): number {
    const visitedHouses = new Set<string>();
    let x = 0;
    let y = 0;

    // Add the starting location to the set of visited houses
    visitedHouses.add(`${x},${y}`);

    // Process each direction
    for (const direction of directions) {
        switch (direction) {
            case '^': y++; break;
            case 'v': y--; break;
            case '>': x++; break;
            case '<': x--; break;
        }
        // Add the current location to the set of visited houses
        visitedHouses.add(`${x},${y}`);
    }

    // Return the number of unique houses visited
    return visitedHouses.size;
}

// Main function
function main() {
    const inputFilePath = 'input.txt';
    const directions = readInput(inputFilePath);
    const result = countHousesWithPresents(directions);
    console.log(result);
}

// Execute the main function
main();