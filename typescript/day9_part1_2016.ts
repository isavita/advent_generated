import * as fs from 'fs';
import * as readline from 'readline';

// Function to read input from a file
async function readInputFile(filePath: string): Promise<string> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let input = '';
    for await (const line of rl) {
        input += line;
    }
    return input;
}

// Function to decompress the input string
function decompress(input: string): number {
    let length = 0;
    let i = 0;

    while (i < input.length) {
        if (input[i] === '(') {
            let j = i;
            while (input[j] !== ')') {
                j++;
            }
            const marker = input.slice(i + 1, j);
            const [A, B] = marker.split('x').map(Number);
            i = j + 1;
            const repeatedSection = input.slice(i, i + A);
            length += repeatedSection.length * B;
            i += A;
        } else {
            length++;
            i++;
        }
    }

    return length;
}

// Main function to read input, decompress it, and print the result
async function main() {
    try {
        const input = await readInputFile('input.txt');
        const decompressedLength = decompress(input);
        console.log(decompressedLength);
    } catch (error) {
        console.error('Error reading the file:', error);
    }
}

main();