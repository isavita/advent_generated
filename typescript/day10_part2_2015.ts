import * as fs from 'fs';

function lookAndSay(input: string): string {
    let result = '';
    let count = 1;

    for (let i = 0; i < input.length; i++) {
        if (input[i] === input[i + 1]) {
            count++;
        } else {
            result += count + input[i];
            count = 1;
        }
    }

    return result;
}

function solve(input: string, iterations: number): number {
    let sequence = input;

    for (let i = 0; i < iterations; i++) {
        sequence = lookAndSay(sequence);
    }

    return sequence.length;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();

    // Part 1: Apply the process 40 times
    const lengthAfter40 = solve(input, 40);
    console.log(`Length after 40 iterations: ${lengthAfter40}`);

    // Part 2: Apply the process 50 times
    const lengthAfter50 = solve(input, 50);
    console.log(`Length after 50 iterations: ${lengthAfter50}`);
}

main();