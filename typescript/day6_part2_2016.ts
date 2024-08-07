import * as fs from 'fs';

function readInput(filePath: string): string[] {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n');
}

function getFrequencies(messages: string[], position: number): Map<string, number> {
    const frequencies = new Map<string, number>();
    for (const message of messages) {
        const char = message[position];
        if (frequencies.has(char)) {
            frequencies.set(char, frequencies.get(char)! + 1);
        } else {
            frequencies.set(char, 1);
        }
    }
    return frequencies;
}

function getMostFrequentCharacter(frequencies: Map<string, number>): string {
    let maxChar = '';
    let maxCount = 0;
    for (const [char, count] of frequencies) {
        if (count > maxCount) {
            maxChar = char;
            maxCount = count;
        }
    }
    return maxChar;
}

function getLeastFrequentCharacter(frequencies: Map<string, number>): string {
    let minChar = '';
    let minCount = Infinity;
    for (const [char, count] of frequencies) {
        if (count < minCount) {
            minChar = char;
            minCount = count;
        }
    }
    return minChar;
}

function main() {
    const messages = readInput('input.txt');
    const messageLength = messages[0].length;

    let mostFrequentMessage = '';
    let leastFrequentMessage = '';

    for (let i = 0; i < messageLength; i++) {
        const frequencies = getFrequencies(messages, i);
        mostFrequentMessage += getMostFrequentCharacter(frequencies);
        leastFrequentMessage += getLeastFrequentCharacter(frequencies);
    }

    console.log('Error-corrected message (most frequent):', mostFrequentMessage);
    console.log('Original message (least frequent):', leastFrequentMessage);
}

main();