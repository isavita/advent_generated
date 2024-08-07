import * as fs from 'fs';

const readInput = (filePath: string): number[] => {
    return fs.readFileSync(filePath, 'utf-8')
             .split('\n')
             .map(Number)
             .filter(Boolean);
};

const calculateJoltageDifferences = (adapters: number[]): number => {
    const sortedAdapters = [0, ...adapters.sort((a, b) => a - b), Math.max(...adapters) + 3];
    const differences: { [key: number]: number } = { 1: 0, 3: 0 };

    for (let i = 1; i < sortedAdapters.length; i++) {
        const diff = sortedAdapters[i] - sortedAdapters[i - 1];
        if (diff in differences) {
            differences[diff]++;
        }
    }

    return differences[1] * differences[3];
};

const main = () => {
    const adapters = readInput('input.txt');
    const result = calculateJoltageDifferences(adapters);
    console.log(result);
};

main();