import * as fs from 'fs';

interface Region {
    geologicIndex: number;
    erosionLevel: number;
    type: number; // 0: rocky, 1: wet, 2: narrow
}

const MODULO = 20183;

function calculateRegionType(depth: number, x: number, y: number, geologicIndex: number): number {
    const erosionLevel = (geologicIndex + depth) % MODULO;
    return erosionLevel % 3; // 0: rocky, 1: wet, 2: narrow
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const depth = parseInt(input[0].split(': ')[1]);
    const [targetX, targetY] = input[1].split(': ')[1].split(',').map(Number);

    const cave: Region[][] = Array.from({ length: targetY + 1 }, () => Array(targetX + 1).fill({ geologicIndex: 0, erosionLevel: 0, type: 0 }));

    for (let y = 0; y <= targetY; y++) {
        for (let x = 0; x <= targetX; x++) {
            let geologicIndex = 0;

            if (x === 0 && y === 0) {
                geologicIndex = 0;
            } else if (x === targetX && y === targetY) {
                geologicIndex = 0;
            } else if (y === 0) {
                geologicIndex = x * 16807;
            } else if (x === 0) {
                geologicIndex = y * 48271;
            } else {
                geologicIndex = cave[y][x - 1].erosionLevel * cave[y - 1][x].erosionLevel;
            }

            const erosionLevel = (geologicIndex + depth) % MODULO;
            const type = calculateRegionType(depth, x, y, geologicIndex);

            cave[y][x] = { geologicIndex, erosionLevel, type };
        }
    }

    const riskLevel = cave.reduce((sum, row) => {
        return sum + row.reduce((rowSum, region) => rowSum + region.type, 0);
    }, 0);

    console.log(riskLevel);
}

main();