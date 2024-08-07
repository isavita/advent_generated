import * as fs from 'fs';

function solveCaptcha(input: string): number {
    const digits = input.split('').map(Number);
    let sum = 0;

    for (let i = 0; i < digits.length; i++) {
        if (digits[i] === digits[(i + 1) % digits.length]) {
            sum += digits[i];
        }
    }

    return sum;
}

function main() {
    fs.readFile('input.txt', 'utf8', (err, data) => {
        if (err) {
            console.error('Error reading file:', err);
            return;
        }

        const result = solveCaptcha(data.trim());
        console.log('The solution to your captcha is:', result);
    });
}

main();