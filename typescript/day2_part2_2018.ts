import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

function calculateChecksum(ids: string[]): number {
    let countTwo = 0;
    let countThree = 0;

    ids.forEach(id => {
        const letterCounts = Array(26).fill(0);
        for (const char of id) {
            letterCounts[char.charCodeAt(0) - 'a'.charCodeAt(0)]++;
        }
        const hasTwo = letterCounts.includes(2);
        const hasThree = letterCounts.includes(3);
        if (hasTwo) countTwo++;
        if (hasThree) countThree++;
    });

    return countTwo * countThree;
}

function findCloseIds(ids: string[]): string | null {
    for (let i = 0; i < ids.length; i++) {
        for (let j = i + 1; j < ids.length; j++) {
            const [diffIndex, diffCount] = compareIds(ids[i], ids[j]);
            if (diffCount === 1) {
                return ids[i].split('').filter((_, index) => index !== diffIndex).join('');
            }
        }
    }
    return null;
}

function compareIds(id1: string, id2: string): [number, number] {
    let diffIndex = -1;
    let diffCount = 0;

    for (let i = 0; i < id1.length; i++) {
        if (id1[i] !== id2[i]) {
            diffCount++;
            if (diffCount > 1) return [-1, diffCount];
            diffIndex = i;
        }
    }
    return [diffIndex, diffCount];
}

const checksum = calculateChecksum(input);
console.log(`Checksum: ${checksum}`);

const commonLetters = findCloseIds(input);
console.log(`Common letters in the correct box IDs: ${commonLetters}`);