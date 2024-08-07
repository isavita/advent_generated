import * as fs from 'fs';
import * as path from 'path';

// Function to read the input file
function readInputFile(filePath: string): string[] {
    const fileContents = fs.readFileSync(filePath, 'utf-8');
    return fileContents.split('\n').filter(line => line.trim() !== '');
}

// Function to find the most frequent character for each position
function findMostFrequentCharacters(lines: string[]): string {
    const numLines = lines.length;
    const numColumns = lines[0].length;
    const frequencyMap: { [key: string]: number[] } = {};

    // Initialize the frequency map
    for (let i = 0; i < numColumns; i++) {
        frequencyMap[i] = Array(26).fill(0); // Assuming only lowercase letters
    }

    // Count the occurrences of each character in each column
    for (let line of lines) {
        for (let i = 0; i < numColumns; i++) {
            const charCode = line.charCodeAt(i) - 'a'.charCodeAt(0);
            frequencyMap[i][charCode]++;
        }
    }

    // Determine the most frequent character for each column
    let result = '';
    for (let i = 0; i < numColumns; i++) {
        let maxFrequency = 0;
        let mostFrequentChar = '';
        for (let j = 0; j < 26; j++) {
            if (frequencyMap[i][j] > maxFrequency) {
                maxFrequency = frequencyMap[i][j];
                mostFrequentChar = String.fromCharCode(j + 'a'.charCodeAt(0));
            }
        }
        result += mostFrequentChar;
    }

    return result;
}

// Main function to execute the program
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const lines = readInputFile(filePath);
    const errorCorrectedMessage = findMostFrequentCharacters(lines);
    console.log(errorCorrectedMessage);
}

// Execute the main function
main();