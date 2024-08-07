import * as fs from 'fs';
import * as crypto from 'crypto';

function md5(input: string): string {
    return crypto.createHash('md5').update(input).digest('hex');
}

function hasTriplet(hash: string): string | null {
    for (let i = 0; i < hash.length - 2; i++) {
        if (hash[i] === hash[i + 1] && hash[i] === hash[i + 2]) {
            return hash[i];
        }
    }
    return null;
}

function hasQuintuplet(hash: string, char: string): boolean {
    const regex = new RegExp(`${char}{5}`);
    return regex.test(hash);
}

function findKeys(salt: string, numKeys: number): number {
    const keys: number[] = [];
    let index = 0;

    while (keys.length < numKeys) {
        const hash = md5(salt + index);
        const tripletChar = hasTriplet(hash);

        if (tripletChar) {
            for (let i = 1; i <= 1000; i++) {
                const futureHash = md5(salt + (index + i));
                if (hasQuintuplet(futureHash, tripletChar)) {
                    keys.push(index);
                    break;
                }
            }
        }

        index++;
    }

    return keys[numKeys - 1];
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    const result = findKeys(input, 64);
    console.log(result);
}

main();