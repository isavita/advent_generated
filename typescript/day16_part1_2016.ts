import * as fs from 'fs';

// Function to generate the dragon curve data
function generateDragonCurve(data: string, length: number): string {
    while (data.length < length) {
        let b = data.split('').reverse().map(c => c === '0' ? '1' : '0').join('');
        data = data + '0' + b;
    }
    return data.slice(0, length);
}

// Function to calculate the checksum
function calculateChecksum(data: string): string {
    let checksum = data;
    while (checksum.length % 2 === 0) {
        let newChecksum = '';
        for (let i = 0; i < checksum.length; i += 2) {
            newChecksum += checksum[i] === checksum[i + 1] ? '1' : '0';
        }
        checksum = newChecksum;
    }
    return checksum;
}

// Main function to read input, process data, and print the checksum
function main() {
    const inputFilePath = 'input.txt';
    const diskLength = 272;

    // Read the initial state from the input file
    const initialState = fs.readFileSync(inputFilePath, 'utf-8').trim();

    // Generate the dragon curve data
    const dragonCurveData = generateDragonCurve(initialState, diskLength);

    // Calculate the checksum
    const checksum = calculateChecksum(dragonCurveData);

    // Print the checksum
    console.log(checksum);
}

// Run the main function
main();