const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const counts = Array.from({ length: 12 }, () => [0, 0]);

input.forEach((num) => {
  for (let i = 0; i < num.length; i++) {
    counts[i][num[i] - '0']++;
  }
});

let gammaRate = 0;
let epsilonRate = 0;

for (let i = 0; i < counts.length; i++) {
  if (counts[i][0] > counts[i][1]) {
    gammaRate |= 1 << (counts.length - i - 1);
  } else {
    epsilonRate |= 1 << (counts.length - i - 1);
  }
}

console.log(gammaRate * epsilonRate);