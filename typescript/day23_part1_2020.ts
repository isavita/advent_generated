const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const cups = new Array(input.length + 1);
let currentCup;
for (let i = 0; i < input.length; i++) {
  const cup = parseInt(input[i]);
  if (i === 0) {
    currentCup = cup;
  }
  if (i < input.length - 1) {
    const nextCup = parseInt(input[i + 1]);
    cups[cup] = nextCup;
  }
}
const firstCup = parseInt(input[0]);
const lastCup = parseInt(input[input.length - 1]);
cups[lastCup] = firstCup;

for (let i = 0; i < 100; i++) {
  const pickup1 = cups[currentCup];
  const pickup2 = cups[pickup1];
  const pickup3 = cups[pickup2];

  cups[currentCup] = cups[pickup3];

  let destinationCup = currentCup - 1;
  if (destinationCup < 1) {
    destinationCup = input.length;
  }
  while (destinationCup === pickup1 || destinationCup === pickup2 || destinationCup === pickup3) {
    destinationCup--;
    if (destinationCup < 1) {
      destinationCup = input.length;
    }
  }

  cups[pickup3] = cups[destinationCup];
  cups[destinationCup] = pickup1;

  currentCup = cups[currentCup];
}

let cup = cups[1];
while (cup !== 1) {
  process.stdout.write(cup.toString());
  cup = cups[cup];
  if (cup === 1) {
    break;
  }
}
console.log();