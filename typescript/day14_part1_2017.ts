import * as fs from 'fs';
import * as crypto from 'crypto';

// Function to compute the knot hash
function knotHash(input: string): string {
    const lengths = input.split('').map(char => char.charCodeAt(0)).concat([17, 31, 73, 47, 23]);
    let list = Array.from({ length: 256 }, (_, i) => i);
    let position = 0;
    let skipSize = 0;

    for (let round = 0; round < 64; round++) {
        for (const length of lengths) {
            let sublist = [];
            for (let i = 0; i < length; i++) {
                sublist.push(list[(position + i) % list.length]);
            }
            sublist.reverse();
            for (let i = 0; i < length; i++) {
                list[(position + i) % list.length] = sublist[i];
            }
            position = (position + length + skipSize) % list.length;
            skipSize++;
        }
    }

    const denseHash = [];
    for (let i = 0; i < 16; i++) {
        denseHash.push(list.slice(i * 16, (i + 1) * 16).reduce((a, b) => a ^ b));
    }

    return denseHash.map(num => num.toString(16).padStart(2, '0')).join('');
}

// Function to convert hexadecimal to binary
function hexToBinary(hex: string): string {
    return hex.split('').map(char => parseInt(char, 16).toString(2).padStart(4, '0')).join('');
}

// Main function to read input, compute hashes, and count used squares
function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    let usedSquaresCount = 0;

    for (let row = 0; row < 128; row++) {
        const hash = knotHash(`${input}-${row}`);
        const binary = hexToBinary(hash);
        usedSquaresCount += binary.split('').filter(bit => bit === '1').length;
    }

    console.log(usedSquaresCount);
}

main();