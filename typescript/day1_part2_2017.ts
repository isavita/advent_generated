import * as fs from 'fs';

function solveCaptchaPartOne(input: string): number {
    let sum = 0;
    const length = input.length;

    for (let i = 0; i < length; i++) {
        if (input[i] === input[(i + 1) % length]) {
            sum += parseInt(input[i]);
        }
    }

    return sum;
}

function solveCaptchaPartTwo(input: string): number {
    let sum = 0;
    const length = input.length;
    const halfLength = length / 2;

    for (let i = 0; i < length; i++) {
        if (input[i] === input[(i + halfLength) % length]) {
            sum += parseInt(input[i]);
        }
    }

    return sum;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();

    const partOneResult = solveCaptchaPartOne(input);
    console.log(`Part One: ${partOneResult}`);

    const partTwoResult = solveCaptchaPartTwo(input);
    console.log(`Part Two: ${partTwoResult}`);
}

main();