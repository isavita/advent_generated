import * as fs from 'fs';
import * as readline from 'readline';

// Function to reverse a sublist in a circular list
function reverseSublist(list: number[], start: number, length: number): void {
    const n = list.length;
    for (let i = 0; i < length / 2; i++) {
        const a = (start + i) % n;
        const b = (start + length - i - 1) % n;
        [list[a], list[b]] = [list[b], list[a]];
    }
}

// Function to perform the knot hash algorithm
function knotHash(lengths: number[]): number {
    const listSize = 256;
    const list: number[] = Array.from({ length: listSize }, (_, i) => i);
    let currentPosition = 0;
    let skipSize = 0;

    for (const length of lengths) {
        reverseSublist(list, currentPosition, length);
        currentPosition = (currentPosition + length + skipSize) % listSize;
        skipSize++;
    }

    return list[0] * list[1];
}

// Function to read input from file and process it
async function processInput(filePath: string): Promise<void> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let input: string = '';
    for await (const line of rl) {
        input += line;
    }

    const lengths = input.split(',').map(Number);
    const result = knotHash(lengths);
    console.log(result);
}

// Entry point of the program
processInput('input.txt').catch(console.error);