const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();
const target = parseInt(data);

let sideLength = Math.ceil(Math.sqrt(target));
if (sideLength % 2 === 0) {
  sideLength++;
}

const maxValue = sideLength * sideLength;
const stepsFromEdge = (sideLength - 1) / 2;
let distanceToMiddle = Infinity;

for (let i = 0; i < 4; i++) {
  const middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
  const distance = Math.abs(target - middlePoint);
  if (distance < distanceToMiddle || i === 0) {
    distanceToMiddle = distance;
  }
}

const manhattanDistance = stepsFromEdge + distanceToMiddle;

console.log(manhattanDistance);