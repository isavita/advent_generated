import * as fs from 'fs';
import * as readline from 'readline';

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let sum = 0;
    for await (const line of rl) {
        const [firstDigit, lastDigit] = findFirstAndLastDigit(line);
        sum += 10 * firstDigit + lastDigit;
    }

    console.log(sum);
}

function findFirstAndLastDigit(line: string): [number, number] {
    const digits = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

    let firstDigit = 0, lastDigit = 0;
    for (let i = 0; i < line.length; i++) {
        const digitStr = line[i];
        if (digitStr >= '0' && digitStr <= '9') {
            const digit = parseInt(digitStr);
            if (firstDigit === 0) {
                firstDigit = digit;
            }
            lastDigit = digit;
        } else {
            for (let j = 0; j < digits.length; j++) {
                if (line.startsWith(digits[j], i)) {
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

main();