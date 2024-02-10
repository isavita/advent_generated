
const fs = require('fs');

function reverseSection(arr, start, length) {
    const n = arr.length;
    for (let i = start, j = start + length - 1; i < j; i++, j--) {
        [arr[i % n], arr[j % n]] = [arr[j % n], arr[i % n]];
    }
}

function knotHash(input) {
    const lengths = [...input].map(char => char.charCodeAt(0));
    lengths.push(17, 31, 73, 47, 23);

    const list = Array.from({ length: 256 }, (_, i) => i);

    let position = 0;
    let skip = 0;
    for (let round = 0; round < 64; round++) {
        for (const length of lengths) {
            reverseSection(list, position, length);
            position += length + skip;
            skip++;
        }
    }

    const denseHash = [];
    for (let i = 0; i < 16; i++) {
        let xor = 0;
        for (let j = 0; j < 16; j++) {
            xor ^= list[i * 16 + j];
        }
        denseHash.push(xor);
    }

    const hexHash = denseHash.map(v => v.toString(16).padStart(2, '0')).join('');
    return hexHash;
}

function hexToBinary(hexStr) {
    let binaryStr = '';
    for (const hexDigit of hexStr) {
        const val = parseInt(hexDigit, 16);
        binaryStr += val.toString(2).padStart(4, '0');
    }
    return binaryStr;
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
let totalUsed = 0;

for (let i = 0; i < 128; i++) {
    const rowKey = `${input}-${i}`;
    const hash = knotHash(rowKey);
    const binaryRow = hexToBinary(hash);

    for (const bit of binaryRow) {
        if (bit === '1') {
            totalUsed++;
        }
    }
}

console.log(totalUsed);
