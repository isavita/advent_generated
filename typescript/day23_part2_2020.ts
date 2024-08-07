const fs = require('fs');

const totalCups = 1000000;
const totalMoves = 10000000;

const input = fs.readFileSync('input.txt', 'utf8');
const cups = new Array(totalCups + 1).fill(0);
let lastCup = 0;

for (let i = 0; i < input.length; i++) {
  const cup = parseInt(input[i]);
  if (i > 0) {
    cups[lastCup] = cup;
  }
  lastCup = cup;
}

for (let i = input.length + 1; i <= totalCups; i++) {
  cups[lastCup] = i;
  lastCup = i;
}
cups[lastCup] = parseInt(input[0]);

let currentCup = parseInt(input[0]);
for (let i = 0; i < totalMoves; i++) {
  const pickup1 = cups[currentCup];
  const pickup2 = cups[pickup1];
  const pickup3 = cups[pickup2];

  cups[currentCup] = cups[pickup3];

  let destinationCup = currentCup - 1;
  if (destinationCup === 0) {
    destinationCup = totalCups;
  }
  while (destinationCup === pickup1 || destinationCup === pickup2 || destinationCup === pickup3) {
    destinationCup--;
    if (destinationCup === 0) {
      destinationCup = totalCups;
    }
  }

  cups[pickup3] = cups[destinationCup];
  cups[destinationCup] = pickup1;

  currentCup = cups[currentCup];
}

const cup1 = cups[1];
const cup2 = cups[cup1];
console.log(cup1 * cup2);