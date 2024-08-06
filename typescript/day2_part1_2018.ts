const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let twoCount = 0;
let threeCount = 0;

input.forEach(id => {
  const charCount = {};
  for (const char of id) {
    charCount[char] = (charCount[char] || 0) + 1;
  }

  let hasTwos = false;
  let hasThrees = false;
  for (const count in charCount) {
    if (charCount[count] === 2) {
      hasTwos = true;
    } else if (charCount[count] === 3) {
      hasThrees = true;
    }
  }

  if (hasTwos) {
    twoCount++;
  }
  if (hasThrees) {
    threeCount++;
  }
});

const checksum = twoCount * threeCount;
console.log(checksum);