import * as fs from 'fs';
import * as readline from 'readline';

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let checksumPart1 = 0;
    let checksumPart2 = 0;

    for await (const line of rl) {
        const numbers = line.split(/\s+/).map(Number);
        checksumPart1 += calculateChecksumPart1(numbers);
        checksumPart2 += calculateChecksumPart2(numbers);
    }

    console.log(`Checksum Part 1: ${checksumPart1}`);
    console.log(`Checksum Part 2: ${checksumPart2}`);
}

function calculateChecksumPart1(numbers: number[]): number {
    const max = Math.max(...numbers);
    const min = Math.min(...numbers);
    return max - min;
}

function calculateChecksumPart2(numbers: number[]): number {
    for (let i = 0; i < numbers.length; i++) {
        for (let j = 0; j < numbers.length; j++) {
            if (i !== j && numbers[i] % numbers[j] === 0) {
                return numbers[i] / numbers[j];
            }
        }
    }
    return 0; // This should never happen based on the problem constraints
}

processLineByLine().catch(console.error);