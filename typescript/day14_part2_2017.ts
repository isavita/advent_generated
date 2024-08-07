import * as fs from 'fs';
import * as crypto from 'crypto';

// Function to create a knot hash
function knotHash(input: string): string {
    const lengths = input.split('').map(x => x.charCodeAt(0)).concat([17, 31, 73, 47, 23]);
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

    let denseHash = '';
    for (let i = 0; i < 16; i++) {
        let xorResult = list[i * 16];
        for (let j = 1; j < 16; j++) {
            xorResult ^= list[i * 16 + j];
        }
        denseHash += xorResult.toString(16).padStart(2, '0');
    }

    return denseHash;
}

// Function to convert hexadecimal to binary string
function hexToBinary(hex: string): string {
    return hex.split('').map(c => parseInt(c, 16).toString(2).padStart(4, '0')).join('');
}

// Function to count used squares
function countUsedSquares(grid: string[]): number {
    let count = 0;
    for (const row of grid) {
        for (const bit of row) {
            if (bit === '1') count++;
        }
    }
    return count;
}

// Function to count regions
function countRegions(grid: string[]): number {
    const visited = Array.from({ length: 128 }, () => new Array(128).fill(false));
    let regionCount = 0;

    const dfs = (x: number, y: number) => {
        if (x < 0 || x >= 128 || y < 0 || y >= 128 || visited[x][y] || grid[x][y] === '0') return;
        visited[x][y] = true;
        dfs(x + 1, y);
        dfs(x - 1, y);
        dfs(x, y + 1);
        dfs(x, y - 1);
    };

    for (let i = 0; i < 128; i++) {
        for (let j = 0; j < 128; j++) {
            if (grid[i][j] === '1' && !visited[i][j]) {
                regionCount++;
                dfs(i, j);
            }
        }
    }

    return regionCount;
}

// Main function
function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    const grid: string[] = [];

    for (let i = 0; i < 128; i++) {
        const hash = knotHash(`${input}-${i}`);
        const binaryRow = hexToBinary(hash);
        grid.push(binaryRow);
    }

    const usedSquares = countUsedSquares(grid);
    const regions = countRegions(grid);

    console.log(`Used squares: ${usedSquares}`);
    console.log(`Regions: ${regions}`);
}

main();