import * as fs from 'fs';

const isValidPassword = (num: number): boolean => {
    const str = num.toString();
    let hasDouble = false;
    let nonDecreasing = true;

    for (let i = 0; i < str.length; i++) {
        if (i > 0) {
            if (str[i] < str[i - 1]) {
                nonDecreasing = false;
                break;
            }
            if (str[i] === str[i - 1]) {
                hasDouble = true;
            }
        }
    }

    return hasDouble && nonDecreasing;
};

const countValidPasswords = (start: number, end: number): number => {
    let count = 0;
    for (let i = start; i <= end; i++) {
        if (isValidPassword(i)) {
            count++;
        }
    }
    return count;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf8');
    const [start, end] = input.split('-').map(Number);
    const result = countValidPasswords(start, end);
    console.log(result);
};

main();