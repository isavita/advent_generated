const fs = require('fs');
const input = fs.readFileSync('input.txt', 'utf8').split('\n');

const slopes = [
  [1, 1],
  [3, 1],
  [5, 1],
  [7, 1],
  [1, 2],
];

let product = 1;
for (const slope of slopes) {
  let treeCount = 0;
  let pos = 0;
  for (let i = 0; i < input.length; i += slope[1]) {
    if (input[i][pos] === '#') {
      treeCount++;
    }
    pos = (pos + slope[0]) % input[i].length;
  }
  product *= treeCount;
}

console.log(product);