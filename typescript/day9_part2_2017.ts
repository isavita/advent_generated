const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
let score = 0;
let depth = 0;
let inGarbage = false;
let cancelNext = false;
let garbageCount = 0;

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
        } else {
            garbageCount++;
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

console.log(garbageCount);