import * as fs from 'fs';
import * as path from 'path';

// Function to read input from file
function readInput(filePath: string): string[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n');
}

// Function to parse the input and generate the blocked ranges
function parseRanges(input: string[]): [number, number][] {
    return input.map(line => {
        const [start, end] = line.split('-').map(Number);
        return [start, end];
    });
}

// Function to find the lowest-valued IP that is not blocked
function findLowestAllowedIP(ranges: [number, number][]): number {
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

// Function to count the total number of allowed IPs
function countAllowedIPs(ranges: [number, number][]): number {
    ranges.sort((a, b) => a[0] - b[0]);
    let currentIP = 0;
    let allowedIPs = 0;
    const maxIP = 4294967295;

    for (const [start, end] of ranges) {
        if (currentIP < start) {
            allowedIPs += start - currentIP;
        }
        currentIP = Math.max(currentIP, end + 1);
    }

    if (currentIP <= maxIP) {
        allowedIPs += maxIP - currentIP + 1;
    }

    return allowedIPs;
}

// Main function
function main() {
    const inputFilePath = path.join(__dirname, 'input.txt');
    const input = readInput(inputFilePath);
    const ranges = parseRanges(input);

    const lowestAllowedIP = findLowestAllowedIP(ranges);
    const allowedIPsCount = countAllowedIPs(ranges);

    console.log(`The lowest-valued IP that is not blocked: ${lowestAllowedIP}`);
    console.log(`The total number of allowed IPs: ${allowedIPsCount}`);
}

main();