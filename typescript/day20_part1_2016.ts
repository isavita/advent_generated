import * as fs from 'fs';

// Function to parse the input file and return the list of blocked IP ranges
function parseInput(filePath: string): [number, number][] {
    const data = fs.readFileSync(filePath, 'utf-8');
    const lines = data.trim().split('\n');
    return lines.map(line => {
        const [start, end] = line.split('-').map(Number);
        return [start, end];
    });
}

// Function to find the lowest-valued IP that is not blocked
function findLowestUnblockedIP(ranges: [number, number][]): number {
    // Sort the ranges by their start values
    ranges.sort((a, b) => a[0] - b[0]);

    let currentIP = 0;
    for (const [start, end] of ranges) {
        if (currentIP < start) {
            return currentIP;
        }
        currentIP = Math.max(currentIP, end + 1);
    }
    return currentIP;
}

// Main function to read input, process it, and print the result
function main() {
    const filePath = 'input.txt';
    const ranges = parseInput(filePath);
    const lowestUnblockedIP = findLowestUnblockedIP(ranges);
    console.log(lowestUnblockedIP);
}

// Run the main function
main();