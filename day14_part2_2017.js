const fs = require('fs');

function reverseSection(arr, start, length) {
    const n = arr.length;
    for (let i = start, j = start + length - 1; i < j; i++, j--) {
        [arr[i % n], arr[j % n]] = [arr[j % n], arr[i % n]];
    }
}

function knotHash(input) {
    let lengths = [];
    for (let char of input) {
        lengths.push(char.charCodeAt(0));
    }
    lengths.push(17, 31, 73, 47, 23);

    let list = Array.from({ length: 256 }, (_, i) => i);

    let position = 0;
    let skip = 0;
    for (let round = 0; round < 64; round++) {
        for (let length of lengths) {
            reverseSection(list, position, length);
            position += length + skip;
            skip++;
        }
    }

    let denseHash = [];
    for (let i = 0; i < 16; i++) {
        let xor = 0;
        for (let j = 0; j < 16; j++) {
            xor ^= list[i * 16 + j];
        }
        denseHash.push(xor);
    }

    let hexHash = denseHash.map(v => v.toString(16).padStart(2, '0')).join('');
    return hexHash;
}

function hexToBinary(hexStr) {
    let binaryStr = '';
    for (let hexDigit of hexStr) {
        let val = parseInt(hexDigit, 16);
        binaryStr += val.toString(2).padStart(4, '0');
    }
    return binaryStr;
}

function dfs(x, y, grid) {
    if (x < 0 || x >= 128 || y < 0 || y >= 128 || grid[x][y] !== 1) {
        return;
    }
    grid[x][y] = 0;
    dfs(x - 1, y, grid);
    dfs(x + 1, y, grid);
    dfs(x, y - 1, grid);
    dfs(x, y + 1, grid);
}

const data = fs.readFileSync('input.txt', 'utf8').trim();
const grid = Array.from({ length: 128 }, () => Array(128).fill(0));
let totalUsed = 0;
let regions = 0;

for (let i = 0; i < 128; i++) {
    const rowKey = `${data}-${i}`;
    const hash = knotHash(rowKey);
    const binaryRow = hexToBinary(hash);

    for (let j = 0; j < 128; j++) {
        if (binaryRow[j] === '1') {
            grid[i][j] = 1;
            totalUsed++;
        }
    }
}

for (let i = 0; i < 128; i++) {
    for (let j = 0; j < 128; j++) {
        if (grid[i][j] === 1) {
            regions++;
            dfs(i, j, grid);
        }
    }
}

console.log(regions);