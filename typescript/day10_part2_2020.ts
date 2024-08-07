import * as fs from 'fs';

const readInput = (filename: string): number[] => {
    const data = fs.readFileSync(filename, 'utf-8');
    return data.split('\n').map(Number).filter(Boolean);
};

const calculateDifferences = (adapters: number[]): [number, number] => {
    const sortedAdapters = [0, ...adapters.sort((a, b) => a - b), adapters[adapters.length - 1] + 3];
    const differences: { [key: number]: number } = { 1: 0, 3: 0 };

    for (let i = 1; i < sortedAdapters.length; i++) {
        const diff = sortedAdapters[i] - sortedAdapters[i - 1];
        differences[diff]++;
    }

    return [differences[1], differences[3]];
};

const countArrangements = (adapters: number[]): number => {
    const sortedAdapters = [0, ...adapters.sort((a, b) => a - b)];
    const ways: number[] = Array(sortedAdapters.length).fill(0);
    ways[0] = 1;

    for (let i = 1; i < sortedAdapters.length; i++) {
        for (let j = 1; j <= 3; j++) {
            if (i - j >= 0 && sortedAdapters[i] - sortedAdapters[i - j] <= 3) {
                ways[i] += ways[i - j];
            }
        }
    }

    return ways[ways.length - 1];
};

const main = () => {
    const adapters = readInput('input.txt');
    const [oneJoltDiffs, threeJoltDiffs] = calculateDifferences(adapters);
    const partOneResult = oneJoltDiffs * threeJoltDiffs;

    console.log(`Part One: ${partOneResult}`);

    const arrangements = countArrangements(adapters);
    console.log(`Part Two: ${arrangements}`);
};

main();