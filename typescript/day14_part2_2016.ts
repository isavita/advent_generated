import * as fs from 'fs';
import * as crypto from 'crypto';

function md5(input: string): string {
    return crypto.createHash('md5').update(input).digest('hex');
}

const hashCache: { [key: string]: string } = {};

function stretchHash(salt: string, index: number, stretchCount: number): string {
    const cacheKey = `${salt}-${index}-${stretchCount}`;
    if (hashCache[cacheKey]) {
        return hashCache[cacheKey];
    }

    let hash = md5(salt + index);
    for (let i = 0; i < stretchCount; i++) {
        hash = md5(hash);
    }

    hashCache[cacheKey] = hash;
    return hash;
}

function hasTriple(hash: string): string | null {
    for (let i = 0; i <= hash.length - 3; i++) {
        if (hash[i] === hash[i + 1] && hash[i] === hash[i + 2]) {
            return hash[i];
        }
    }
    return null;
}

function hasQuintuple(hash: string, char: string): boolean {
    const quintuple = char.repeat(5);
    return hash.includes(quintuple);
}

function findKeys(salt: string, stretchCount: number, keyCount: number): number {
    const keys: number[] = [];
    let index = 0;

    while (keys.length < keyCount) {
        const hash = stretchHash(salt, index, stretchCount);
        const tripleChar = hasTriple(hash);

        if (tripleChar) {
            let isKey = false;
            for (let i = 1; i <= 1000; i++) {
                const futureHash = stretchHash(salt, index + i, stretchCount);
                if (hasQuintuple(futureHash, tripleChar)) {
                    isKey = true;
                    break;
                }
            }
            if (isKey) {
                keys.push(index);
            }
        }

        index++;
    }

    return keys[keyCount - 1];
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8').trim();
    const stretchCount = 2016;
    const keyCount = 64;

    const index = findKeys(input, stretchCount, keyCount);
    console.log(`The index that produces the 64th one-time pad key is: ${index}`);
}

main();