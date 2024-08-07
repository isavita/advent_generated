import { readFileSync } from 'fs';

const input = readFileSync('input.txt', 'utf-8').trim();
const startingNumbers = input.split(',').map(Number);

const find2020thNumber = (numbers: number[], turns: number): number => {
    const lastSpoken: Map<number, number> = new Map();
    let currentNumber: number = 0; // Initialize to a default value

    numbers.forEach((num, index) => {
        lastSpoken.set(num, index + 1);
        currentNumber = num;
    });

    for (let turn = numbers.length + 1; turn <= turns; turn++) {
        const lastTurn = lastSpoken.get(currentNumber);
        lastSpoken.set(currentNumber, turn - 1);
        currentNumber = lastTurn === undefined ? 0 : turn - 1 - lastTurn;
    }

    return currentNumber;
};

console.log(find2020thNumber(startingNumbers, 2020));