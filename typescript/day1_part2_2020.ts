import * as fs from 'fs';

const findTwoEntriesProduct = (numbers: number[]): number | null => {
    const seen = new Set<number>();
    for (const number of numbers) {
        const complement = 2020 - number;
        if (seen.has(complement)) {
            return number * complement;
        }
        seen.add(number);
    }
    return null;
};

const findThreeEntriesProduct = (numbers: number[]): number | null => {
    for (let i = 0; i < numbers.length; i++) {
        const seen = new Set<number>();
        const target = 2020 - numbers[i];
        for (let j = i + 1; j < numbers.length; j++) {
            const complement = target - numbers[j];
            if (seen.has(complement)) {
                return numbers[i] * numbers[j] * complement;
            }
            seen.add(numbers[j]);
        }
    }
    return null;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const numbers = input.split('\n').map(Number).filter(n => !isNaN(n));

    const productOfTwo = findTwoEntriesProduct(numbers);
    const productOfThree = findThreeEntriesProduct(numbers);

    console.log('Product of two entries:', productOfTwo);
    console.log('Product of three entries:', productOfThree);
};

main();