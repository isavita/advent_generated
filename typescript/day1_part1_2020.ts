import { readFileSync } from 'fs';

const findProductOfEntries = (target: number): number => {
    const expenses = new Set<number>();
    const data = readFileSync('input.txt', 'utf-8').split('\n').map(Number);

    for (const entry of data) {
        const complement = target - entry;
        if (expenses.has(complement)) {
            return entry * complement;
        }
        expenses.add(entry);
    }
    throw new Error("No two entries sum to the target.");
};

try {
    const result = findProductOfEntries(2020);
    console.log(result);
} catch (error: unknown) {
    if (error instanceof Error) {
        console.error(error.message);
    } else {
        console.error("An unknown error occurred.");
    }
}