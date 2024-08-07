import * as fs from 'fs';
import * as path from 'path';

// Function to read input from a file
function readInput(filePath: string): string {
    return fs.readFileSync(filePath, 'utf-8').trim();
}

// Function to decompress the input string
function decompressLength(input: string): number {
    let length = 0;
    let i = 0;

    while (i < input.length) {
        if (input[i] === '(') {
            let j = i;
            while (input[j] !== ')') j++;
            const marker = input.slice(i + 1, j);
            const [count, repeat] = marker.split('x').map(Number);
            i = j + 1;
            const sequence = input.slice(i, i + count);
            i += count;
            length += decompressLength(sequence) * repeat;
        } else {
            length++;
            i++;
        }
    }

    return length;
}

// Main function to execute the program
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const input = readInput(filePath);
    const decompressedLength = decompressLength(input);
    console.log(decompressedLength);
}

main();