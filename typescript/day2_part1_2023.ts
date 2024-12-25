
import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
let totalSum = 0;

for (const line of input) {
  const gameId = parseInt(line.match(/Game (\d+):/)![1]);
  const rounds = line.split(':')[1].split(';');
  let isValid = true;
  let maxRed = 0;
  let maxGreen = 0;
  let maxBlue = 0;

  for (const round of rounds) {
    const cubes = round.match(/(\d+) (red|green|blue)/g);
    if (!cubes) continue;

    for (const cube of cubes) {
      const [countStr, color] = cube.split(' ');
      const count = parseInt(countStr);

      if (color === 'red') maxRed = Math.max(maxRed, count);
      if (color === 'green') maxGreen = Math.max(maxGreen, count);
      if (color === 'blue') maxBlue = Math.max(maxBlue, count);
    }
  }

  if (maxRed <= 12 && maxGreen <= 13 && maxBlue <= 14) {
    totalSum += gameId;
  }
}

console.log(totalSum);
