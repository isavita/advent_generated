import * as fs from 'fs';

const calculatePowerLevel = (x: number, y: number, serialNumber: number): number => {
    const rackID = x + 10;
    let powerLevel = rackID * y + serialNumber;
    powerLevel = (powerLevel * rackID);
    const hundredsDigit = Math.floor(Math.abs(powerLevel) / 100) % 10;
    return hundredsDigit - 5;
};

const createGrid = (serialNumber: number): number[][] => {
    const grid: number[][] = Array.from({ length: 300 }, () => Array(300).fill(0));
    for (let x = 1; x <= 300; x++) {
        for (let y = 1; y <= 300; y++) {
            grid[x - 1][y - 1] = calculatePowerLevel(x, y, serialNumber);
        }
    }
    return grid;
};

const createPrefixSum = (grid: number[][]): number[][] => {
    const prefixSum: number[][] = Array.from({ length: 301 }, () => Array(301).fill(0));
    for (let x = 1; x <= 300; x++) {
        for (let y = 1; y <= 300; y++) {
            prefixSum[x][y] = grid[x - 1][y - 1] +
                prefixSum[x - 1][y] +
                prefixSum[x][y - 1] -
                prefixSum[x - 1][y - 1];
        }
    }
    return prefixSum;
};

const findMaxPowerSquare = (prefixSum: number[][], maxSize: number): { x: number, y: number, size: number, power: number } => {
    let maxPower = -Infinity;
    let bestSquare = { x: 0, y: 0, size: 0, power: 0 };

    for (let size = 1; size <= maxSize; size++) {
        for (let x = size; x <= 300; x++) {
            for (let y = size; y <= 300; y++) {
                const totalPower = prefixSum[x][y] -
                    prefixSum[x - size][y] -
                    prefixSum[x][y - size] +
                    prefixSum[x - size][y - size];
                
                if (totalPower > maxPower) {
                    maxPower = totalPower;
                    bestSquare = { x: x - size + 1, y: y - size + 1, size, power: totalPower };
                }
            }
        }
    }
    return bestSquare;
};

const main = () => {
    const serialNumber = parseInt(fs.readFileSync('input.txt', 'utf-8').trim(), 10);
    const grid = createGrid(serialNumber);
    const prefixSum = createPrefixSum(grid);
    
    const resultPart1 = findMaxPowerSquare(prefixSum, 3);
    console.log(`${resultPart1.x},${resultPart1.y}`);

    const resultPart2 = findMaxPowerSquare(prefixSum, 300);
    console.log(`${resultPart2.x},${resultPart2.y},${resultPart2.size}`);
};

main();