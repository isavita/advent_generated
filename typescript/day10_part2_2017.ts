import * as fs from 'fs';

function knotHash(input: string): string {
    const lengths = input.split('').map(char => char.charCodeAt(0)).concat([17, 31, 73, 47, 23]);
    const listSize = 256;
    let list = Array.from({ length: listSize }, (_, i) => i);
    let currentPosition = 0;
    let skipSize = 0;

    for (let round = 0; round < 64; round++) {
        for (const length of lengths) {
            reverseSublist(list, currentPosition, length);
            currentPosition = (currentPosition + length + skipSize) % listSize;
            skipSize++;
        }
    }

    const denseHash = [];
    for (let i = 0; i < 16; i++) {
        let xorResult = list[i * 16];
        for (let j = 1; j < 16; j++) {
            xorResult ^= list[i * 16 + j];
        }
        denseHash.push(xorResult);
    }

    return denseHash.map(num => num.toString(16).padStart(2, '0')).join('');
}

function reverseSublist(list: number[], start: number, length: number): void {
    const listSize = list.length;
    for (let i = 0; i < length / 2; i++) {
        const a = (start + i) % listSize;
        const b = (start + length - 1 - i) % listSize;
        [list[a], list[b]] = [list[b], list[a]];
    }
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const hash = knotHash(input);
    console.log(hash);
}

main();