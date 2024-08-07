import * as fs from 'fs';

function processStream(input: string): [number, number] {
    let score = 0;
    let totalScore = 0;
    let garbageCount = 0;
    let inGarbage = false;
    let cancelNext = false;
    let depth = 0;

    for (let i = 0; i < input.length; i++) {
        const char = input[i];

        if (cancelNext) {
            cancelNext = false;
            continue;
        }

        if (char === '!') {
            cancelNext = true;
            continue;
        }

        if (inGarbage) {
            if (char === '>') {
                inGarbage = false;
            } else {
                garbageCount++;
            }
        } else {
            if (char === '<') {
                inGarbage = true;
            } else if (char === '{') {
                depth++;
            } else if (char === '}') {
                score += depth;
                depth--;
            }
        }
    }

    return [score, garbageCount];
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf8');
    const [totalScore, garbageCount] = processStream(input);

    console.log(`Total score for all groups: ${totalScore}`);
    console.log(`Non-canceled characters within the garbage: ${garbageCount}`);
}

main();