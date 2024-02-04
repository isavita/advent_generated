const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

const digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

let sum = 0;

data.forEach(line => {
    const [firstDigit, lastDigit] = findFirstAndLastDigit(line);
    sum += 10 * firstDigit + lastDigit;
});

console.log(sum);

function findFirstAndLastDigit(line) {
    let firstDigit = 0;
    let lastDigit = 0;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];
        const digitStr = char;

        if (!isNaN(digitStr)) {
            if (firstDigit === 0) {
                firstDigit = parseInt(digitStr);
            }
            lastDigit = parseInt(digitStr);
        } else {
            for (let j = 0; j < digits.length; j++) {
                const digit = digits[j];
                if (line.substring(i).startsWith(digit)) {
                    if (firstDigit === 0) {
                        firstDigit = j;
                    }
                    lastDigit = j;
                    break;
                }
            }
        }
    }

    return [firstDigit, lastDigit];
}