import * as fs from 'fs';
import * as readline from 'readline';

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let totalScore = 0;
    let currentScore = 0;
    let inGarbage = false;
    let ignoreNext = false;

    for await (const line of rl) {
        for (const char of line) {
            if (ignoreNext) {
                ignoreNext = false;
                continue;
            }

            if (inGarbage) {
                if (char === '>') {
                    inGarbage = false;
                } else if (char === '!') {
                    ignoreNext = true;
                }
            } else {
                if (char === '{') {
                    currentScore++;
                    totalScore += currentScore;
                } else if (char === '}') {
                    currentScore--;
                } else if (char === '<') {
                    inGarbage = true;
                }
            }
        }
    }

    console.log(`Total score for all groups: ${totalScore}`);
}

processLineByLine().catch(console.error);