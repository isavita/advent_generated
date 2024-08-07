import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

const parseRange = (range: string): [number, number] => {
    const [start, end] = range.split('-').map(Number);
    return [start, end];
};

const contains = (range1: [number, number], range2: [number, number]): boolean => {
    return range1[0] <= range2[0] && range1[1] >= range2[1];
};

const overlaps = (range1: [number, number], range2: [number, number]): boolean => {
    return range1[0] <= range2[1] && range2[0] <= range1[1];
};

const countPairs = (input: string[]) => {
    let fullyContainedCount = 0;
    let overlapCount = 0;

    for (const line of input) {
        const [range1Str, range2Str] = line.split(',');
        const range1 = parseRange(range1Str);
        const range2 = parseRange(range2Str);

        if (contains(range1, range2) || contains(range2, range1)) {
            fullyContainedCount++;
        }
        if (overlaps(range1, range2)) {
            overlapCount++;
        }
    }

    return { fullyContainedCount, overlapCount };
};

const { fullyContainedCount, overlapCount } = countPairs(input);
console.log(`Fully contained pairs: ${fullyContainedCount}`);
console.log(`Overlapping pairs: ${overlapCount}`);