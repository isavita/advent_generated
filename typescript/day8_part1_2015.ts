
const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf-8');
const lines = file.split('\n');

let totalDiff = 0;
for (const line of lines) {
  const codeLength = line.length;
  let memoryLength = 0;
  let inEscape = false;
  let hexCount = 0;

  for (let i = 1; i < line.length - 1; i++) {
    if (hexCount > 0) {
      hexCount--;
    } else if (inEscape) {
      if (line[i] === 'x') {
        hexCount = 2;
      }
      inEscape = false;
      memoryLength++;
    } else if (line[i] === '\\') {
      inEscape = true;
    } else {
      memoryLength++;
    }
  }

  totalDiff += codeLength - memoryLength;
}

console.log(totalDiff);
