import * as fs from 'fs';

// Define the Disc interface
interface Disc {
    positions: number;
    startPosition: number;
}

// Read input from file
const input = fs.readFileSync('input.txt', 'utf-8');

// Parse the input to get the discs
const discs: Disc[] = input.trim().split('\n').map(line => {
    const match = line.match(/Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)/);
    if (match) {
        const [, discNumber, positions, startPosition] = match;
        return { positions: parseInt(positions, 10), startPosition: parseInt(startPosition, 10) };
    }
    throw new Error('Invalid input format');
});

// Function to find the first time the capsule can pass through all discs
function findFirstTime(discs: Disc[]): number {
    let time = 0;
    while (true) {
        let canPass = true;
        for (let i = 0; i < discs.length; i++) {
            const disc = discs[i];
            const discTime = time + i + 1;
            if ((disc.startPosition + discTime) % disc.positions !== 0) {
                canPass = false;
                break;
            }
        }
        if (canPass) {
            return time;
        }
        time++;
    }
}

// Find and print the first time
const firstTime = findFirstTime(discs);
console.log(firstTime);