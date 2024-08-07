import * as fs from 'fs';

// Constants for the generators
const GEN_A_FACTOR = 16807;
const GEN_B_FACTOR = 48271;
const MODULO = 2147483647;
const GEN_A_MASK = 0xFFFF;
const PAIR_COUNT = 40000000;

function generateNextValue(previousValue: number, factor: number): number {
    return (previousValue * factor) % MODULO;
}

function countMatchingPairs(startA: number, startB: number): number {
    let valueA = startA;
    let valueB = startB;
    let matchCount = 0;

    for (let i = 0; i < PAIR_COUNT; i++) {
        valueA = generateNextValue(valueA, GEN_A_FACTOR);
        valueB = generateNextValue(valueB, GEN_B_FACTOR);

        if ((valueA & GEN_A_MASK) === (valueB & GEN_A_MASK)) {
            matchCount++;
        }
    }

    return matchCount;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const startA = parseInt(input[0], 10);
    const startB = parseInt(input[1], 10);

    const result = countMatchingPairs(startA, startB);
    console.log(result);
}

main();