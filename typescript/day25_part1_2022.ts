import * as fs from 'fs';

const snafuToDecimal = (snafu: string): number => {
    const mapping: { [key: string]: number } = { '2': 2, '1': 1, '0': 0, '-': -1, '=': -2 };
    let total = 0;
    const length = snafu.length;

    for (let i = 0; i < length; i++) {
        const digit = snafu[length - 1 - i];
        total += mapping[digit] * Math.pow(5, i);
    }

    return total;
};

const decimalToSnafu = (num: number): string => {
    const digits: string[] = [];
    while (num !== 0) {
        let remainder = num % 5;
        num = Math.floor(num / 5);
        if (remainder > 2) {
            remainder -= 5;
            num += 1;
        }
        digits.push(remainder === 2 ? '2' : remainder === 1 ? '1' : remainder === 0 ? '0' : remainder === -1 ? '-' : '=');
    }
    return digits.reverse().join('');
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const total = input.reduce((sum, line) => sum + snafuToDecimal(line), 0);
    const result = decimalToSnafu(total);
    console.log(result);
};

main();