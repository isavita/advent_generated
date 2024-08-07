import * as fs from 'fs';

const input = parseInt(fs.readFileSync('input.txt', 'utf8').trim());

function spinlockPart1(steps: number, iterations: number): number {
    const buffer = [0];
    let currentPosition = 0;

    for (let i = 1; i <= iterations; i++) {
        currentPosition = (currentPosition + steps) % buffer.length;
        buffer.splice(currentPosition + 1, 0, i);
        currentPosition++;
    }

    return buffer[(currentPosition + 1) % buffer.length];
}

function spinlockPart2(steps: number, iterations: number): number {
    let currentPosition = 0;
    let valueAfterZero = 0;

    for (let i = 1; i <= iterations; i++) {
        currentPosition = (currentPosition + steps) % i + 1;
        if (currentPosition === 1) {
            valueAfterZero = i;
        }
    }

    return valueAfterZero;
}

const part1Result = spinlockPart1(input, 2017);
const part2Result = spinlockPart2(input, 50000000);

console.log(`Part 1 result: ${part1Result}`);
console.log(`Part 2 result: ${part2Result}`);