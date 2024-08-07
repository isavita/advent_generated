import * as fs from 'fs';

const hash = (input: string): number => {
    let currentValue = 0;
    for (const char of input) {
        currentValue += char.charCodeAt(0);
        currentValue = (currentValue * 17) % 256;
    }
    return currentValue;
};

const main = () => {
    const data = fs.readFileSync('input.txt', 'utf8').replace(/\n/g, '');
    const steps = data.split(',');
    const resultSum = steps.reduce((sum, step) => sum + hash(step), 0);
    console.log(resultSum);
};

main();