const fs = require('fs');

const ElemToMatch = 'ZZZ';

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim();
  const lines = input.split('\n');

  const desertMap = {};

  // Ignore the first two lines since they are the instruction set and a blank line
  for (let i = 2; i < lines.length; i++) {
    if (lines[i].length === 0) {
      continue;
    }

    const matches = lines[i].match(/[A-Z]{3}/g);
    desertMap[matches[0]] = {
      left: matches[1],
      right: matches[2],
    };
  }

  let current = 'AAA';
  let steps = 0;

  while (current !== ElemToMatch) {
    for (let i = 0; i < lines[0].trim().length; i++) {
      const direction = lines[0][i];
      if (direction === 'R') {
        current = desertMap[current].right;
      } else if (direction === 'L') {
        current = desertMap[current].left;
      }
      steps++;

      if (current === ElemToMatch) {
        break;
      }
    }
  }

  console.log(steps);
}

main();