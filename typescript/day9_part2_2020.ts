import * as fs from 'fs';

const inputFile = 'input.txt';
const preambleLength = 25;

const findInvalidNumber = (numbers: number[]): number => {
    for (let i = preambleLength; i < numbers.length; i++) {
        const preamble = new Set(numbers.slice(i - preambleLength, i));
        let isValid = false;

        for (const num of preamble) {
            if (preamble.has(numbers[i] - num) && (numbers[i] - num) !== num) {
                isValid = true;
                break;
            }
        }

        if (!isValid) {
            return numbers[i];
        }
    }
    return -1; // Should not reach here if input is valid
};

const findEncryptionWeakness = (numbers: number[], target: number): number => {
    for (let start = 0; start < numbers.length; start++) {
        let sum = 0;
        for (let end = start; end < numbers.length; end++) {
            sum += numbers[end];
            if (sum === target) {
                const range = numbers.slice(start, end + 1);
                return Math.min(...range) + Math.max(...range);
            }
            if (sum > target) break;
        }
    }
    return -1; // Should not reach here if input is valid
};

const main = () => {
    const data = fs.readFileSync(inputFile, 'utf-8');
    const numbers = data.split('\n').map(Number).filter(Boolean);

    const invalidNumber = findInvalidNumber(numbers);
    console.log(`First invalid number: ${invalidNumber}`);

    const encryptionWeakness = findEncryptionWeakness(numbers, invalidNumber);
    console.log(`Encryption weakness: ${encryptionWeakness}`);
};

main();