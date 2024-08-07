import * as fs from 'fs';

const readInput = (filePath: string): number[] => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.split('\n').map(Number).filter(Boolean);
};

const countIncreases = (depths: number[]): number => {
    let count = 0;
    for (let i = 1; i < depths.length; i++) {
        if (depths[i] > depths[i - 1]) count++;
    }
    return count;
};

const countSlidingWindowIncreases = (depths: number[]): number => {
    let count = 0;
    for (let i = 3; i < depths.length; i++) {
        const prevSum = depths[i - 3] + depths[i - 2] + depths[i - 1];
        const currSum = depths[i - 2] + depths[i - 1] + depths[i];
        if (currSum > prevSum) count++;
    }
    return count;
};

const depths = readInput('input.txt');
const partOneResult = countIncreases(depths);
const partTwoResult = countSlidingWindowIncreases(depths);

console.log(`Part One: ${partOneResult}`);
console.log(`Part Two: ${partTwoResult}`);