import * as fs from 'fs';

const FACTOR_A = 16807;
const FACTOR_B = 48271;
const MOD_VALUE = 2147483647;
const LOW_16_BITS_MASK = 0xFFFF;

function readInput(filePath: string): number[] {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n').map(Number);
    if (data.length !== 2) {
        throw new Error('Input file must contain exactly two lines with starting values.');
    }
    return data;
}

function* generateValues(startValue: number, factor: number, criteria: number): IterableIterator<number> {
    let value = startValue;
    while (true) {
        do {
            value = (value * factor) % MOD_VALUE;
        } while (value % criteria !== 0);
        yield value;
    }
}

function countMatchingPairs(startA: number, startB: number, pairsToConsider: number, criteriaA: number, criteriaB: number): number {
    const genA = generateValues(startA, FACTOR_A, criteriaA);
    const genB = generateValues(startB, FACTOR_B, criteriaB);

    let matchCount = 0;
    for (let i = 0; i < pairsToConsider; i++) {
        const valueA = genA.next().value;
        const valueB = genB.next().value;
        if ((valueA & LOW_16_BITS_MASK) === (valueB & LOW_16_BITS_MASK)) {
            matchCount++;
        }
    }

    return matchCount;
}

function main() {
    const [startA, startB] = readInput('input.txt');

    // Part 1
    const part1MatchCount = countMatchingPairs(startA, startB, 40_000_000, 1, 1);
    console.log(`Part 1: ${part1MatchCount}`);

    // Part 2
    const part2MatchCount = countMatchingPairs(startA, startB, 5_000_000, 4, 8);
    console.log(`Part 2: ${part2MatchCount}`);
}

main();