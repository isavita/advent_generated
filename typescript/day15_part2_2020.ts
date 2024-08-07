import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim();
const startingNumbers = input.split(',').map(Number);

function getNthSpokenNumber(numbers: number[], n: number): number {
    const lastSpoken: Map<number, number> = new Map();
    numbers.forEach((num, index) => lastSpoken.set(num, index + 1));

    let lastNumber = numbers[numbers.length - 1];
    for (let turn = numbers.length + 1; turn <= n; turn++) {
        const nextNumber = lastSpoken.has(lastNumber) ? turn - 1 - lastSpoken.get(lastNumber)! : 0;
        lastSpoken.set(lastNumber, turn - 1);
        lastNumber = nextNumber;
    }
    return lastNumber;
}

const number2020 = getNthSpokenNumber(startingNumbers, 2020);
const number30000000 = getNthSpokenNumber(startingNumbers, 30000000);

console.log(`2020th number spoken: ${number2020}`);
console.log(`30000000th number spoken: ${number30000000}`);