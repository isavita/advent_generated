import { readFileSync } from 'fs';

const input = parseInt(readFileSync('input.txt', 'utf-8').trim());

const spinlock = (steps: number, iterations: number): number => {
    const buffer: number[] = [0];
    let currentPosition = 0;

    for (let i = 1; i <= iterations; i++) {
        currentPosition = (currentPosition + steps) % buffer.length;
        buffer.splice(currentPosition + 1, 0, i);
        currentPosition++;
    }

    const indexAfterLast = (buffer.indexOf(iterations) + 1) % buffer.length;
    return buffer[indexAfterLast];
};

console.log(spinlock(input, 2017));