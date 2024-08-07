import * as fs from 'fs';

function isValidPassword(password: number): boolean {
    const str = password.toString();
    let hasDouble = false;
    let isNonDecreasing = true;

    for (let i = 0; i < str.length; i++) {
        if (i > 0 && str[i] < str[i - 1]) {
            isNonDecreasing = false;
            break;
        }
        if (str[i] === str[i - 1]) {
            hasDouble = true;
        }
    }

    return hasDouble && isNonDecreasing;
}

function isValidPasswordPartTwo(password: number): boolean {
    const str = password.toString();
    let hasExactDouble = false;
    let isNonDecreasing = true;
    let count = 1;

    for (let i = 0; i < str.length; i++) {
        if (i > 0) {
            if (str[i] < str[i - 1]) {
                isNonDecreasing = false;
                break;
            }
            if (str[i] === str[i - 1]) {
                count++;
            } else {
                if (count === 2) hasExactDouble = true;
                count = 1;
            }
        }
    }
    if (count === 2) hasExactDouble = true;

    return hasExactDouble && isNonDecreasing;
}

function countValidPasswords(range: [number, number], validate: (password: number) => boolean): number {
    let count = 0;
    for (let i = range[0]; i <= range[1]; i++) {
        if (validate(i)) count++;
    }
    return count;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const [min, max] = input.split('-').map(Number) as [number, number];

    const partOneCount = countValidPasswords([min, max], isValidPassword);
    const partTwoCount = countValidPasswords([min, max], isValidPasswordPartTwo);

    console.log(`Part One: ${partOneCount}`);
    console.log(`Part Two: ${partTwoCount}`);
}

main();