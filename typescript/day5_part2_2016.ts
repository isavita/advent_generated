import * as fs from 'fs';
import * as crypto from 'crypto';

function md5(input: string): string {
    return crypto.createHash('md5').update(input).digest('hex');
}

function findPassword(doorId: string, partTwo: boolean = false): string {
    const passwordLength = 8;
    let password: string | Array<string | null> = partTwo ? Array(passwordLength).fill(null) : '';
    let index = 0;
    let count = 0;

    while (count < passwordLength) {
        const hash = md5(`${doorId}${index}`);
        if (hash.startsWith('00000')) {
            if (partTwo) {
                const position = parseInt(hash[5], 16);
                const character = hash[6];
                if (!isNaN(position) && position >= 0 && position < passwordLength && (password as Array<string | null>)[position] === null) {
                    (password as Array<string | null>)[position] = character;
                    count++;
                }
            } else {
                password += hash[5];
                count++;
            }
        }
        index++;
    }

    return partTwo ? (password as Array<string | null>).join('') : password as string;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();

    const passwordPartOne = findPassword(input, false);
    console.log(`Part One Password: ${passwordPartOne}`);

    const passwordPartTwo = findPassword(input, true);
    console.log(`Part Two Password: ${passwordPartTwo}`);
}

main();