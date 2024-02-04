const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

let lengths = [];
for (let i = 0; i < input.length; i++) {
    lengths.push(input.charCodeAt(i));
}
lengths = lengths.concat([17, 31, 73, 47, 23]);

let list = Array.from({ length: 256 }, (_, i) => i);
let currentPosition = 0;
let skipSize = 0;

for (let round = 0; round < 64; round++) {
    for (let length of lengths) {
        for (let i = 0; i < length / 2; i++) {
            let start = (currentPosition + i) % 256;
            let end = (currentPosition + length - 1 - i) % 256;
            [list[start], list[end]] = [list[end], list[start]];
        }
        currentPosition = (currentPosition + length + skipSize) % 256;
        skipSize++;
    }
}

let denseHash = [];
for (let i = 0; i < 256; i += 16) {
    let xor = 0;
    for (let j = 0; j < 16; j++) {
        xor ^= list[i + j];
    }
    denseHash.push(xor);
}

let hexHash = Buffer.from(denseHash).toString('hex');

console.log(hexHash);