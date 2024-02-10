const fs = require('fs');

let score = 0;
let depth = 0;
let inGarbage = false;
let cancelNext = false;

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

data.forEach(line => {
  for (let i = 0; i < line.length; i++) {
    const ch = line[i];
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
});

console.log(score);