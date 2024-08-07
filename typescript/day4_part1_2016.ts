import * as fs from 'fs';

// Function to read the input file
function readInputFile(filename: string): string[] {
    return fs.readFileSync(filename, 'utf-8').split('\n');
}

// Function to validate if a room is real
function isRealRoom(room: string): boolean {
    const match = room.match(/^(.*)-(\d+)\[([a-z]+)\]$/);
    if (!match) return false;

    const [, encryptedName, sectorIdStr, checksum] = match;
    const name = encryptedName.replace(/-/g, '');
    const sectorId = parseInt(sectorIdStr, 10);

    // Count the frequency of each letter in the encrypted name
    const letterCounts: { [key: string]: number } = {};
    for (const char of name) {
        letterCounts[char] = (letterCounts[char] || 0) + 1;
    }

    // Sort letters by frequency and then alphabetically
    const sortedLetters = Object.keys(letterCounts).sort((a, b) => {
        if (letterCounts[b] !== letterCounts[a]) {
            return letterCounts[b] - letterCounts[a];
        }
        return a.localeCompare(b);
    });

    // Generate the checksum from the sorted letters
    const generatedChecksum = sortedLetters.slice(0, 5).join('');

    // Check if the generated checksum matches the provided checksum
    return generatedChecksum === checksum;
}

// Main function to process the input and calculate the sum of sector IDs
function main() {
    const inputLines = readInputFile('input.txt');
    let sumOfSectorIds = 0;

    for (const line of inputLines) {
        if (isRealRoom(line)) {
            const sectorId = parseInt(line.match(/\d+/)![0], 10);
            sumOfSectorIds += sectorId;
        }
    }

    console.log(sumOfSectorIds);
}

main();