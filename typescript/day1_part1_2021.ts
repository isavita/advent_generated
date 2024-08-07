import * as fs from 'fs';

const countIncreases = (depths: number[]): number => {
    return depths.reduce((count, depth, index) => {
        if (index > 0 && depth > depths[index - 1]) {
            return count + 1;
        }
        return count;
    }, 0);
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const depths = input.split('\n').map(Number).filter(Boolean);
    const increaseCount = countIncreases(depths);
    console.log(increaseCount);
};

main();