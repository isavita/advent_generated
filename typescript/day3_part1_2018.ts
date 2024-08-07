import * as fs from 'fs';

const fabricSize = 1000;
const fabric: number[][] = Array.from({ length: fabricSize }, () => Array(fabricSize).fill(0));

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

input.forEach(line => {
    const match = line.match(/#\d+ @ (\d+),(\d+): (\d+)x(\d+)/);
    if (match) {
        const left = parseInt(match[1]);
        const top = parseInt(match[2]);
        const width = parseInt(match[3]);
        const height = parseInt(match[4]);

        for (let i = top; i < top + height; i++) {
            for (let j = left; j < left + width; j++) {
                fabric[i][j]++;
            }
        }
    }
});

const overlappingArea = fabric.flat().filter(count => count > 1).length;

console.log(overlappingArea);