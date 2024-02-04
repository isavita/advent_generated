const fs = require('fs');

// Step 1: Read Input
const input = fs.readFileSync('input.txt', 'utf8').trim();

// Step 2: Initialize Variables
let score = 0;
let depth = 0;
let inGarbage = false;
let cancelNext = false;

// Step 3: Process Stream
for (let i = 0; i < input.length; i++) {
    const ch = input[i];

    if (cancelNext) {
        cancelNext = false;
        continue;
    }

    if (inGarbage) {
        if (ch === '!') {
            cancelNext = true;
        } else if (ch === '>') {
            inGarbage = false;
        }
    } else {
        switch (ch) {
            case '{':
                depth++;
                break;
            case '}':
                score += depth;
                depth--;
                break;
            case '<':
                inGarbage = true;
                break;
        }
    }
}

// Step 4: Print Score
console.log(score);