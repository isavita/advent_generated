import * as fs from 'fs';

function calculateChecksum(boxIds: string[]): number {
    let countTwo = 0;
    let countThree = 0;

    for (const id of boxIds) {
        const letterCounts = new Map<string, number>();
        
        for (const letter of id) {
            letterCounts.set(letter, (letterCounts.get(letter) || 0) + 1);
        }

        const counts = Array.from(letterCounts.values());
        if (counts.includes(2)) countTwo++;
        if (counts.includes(3)) countThree++;
    }

    return countTwo * countThree;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const boxIds = input.trim().split('\n');
    const checksum = calculateChecksum(boxIds);
    console.log(checksum);
}

main();