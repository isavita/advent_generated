const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

const discs = input.map(line => {
  const [, , totalPositions, startPosition] = line.match(/Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+)./);
  return { totalPositions: +totalPositions, startPosition: +startPosition };
});

let time = 0;
while (true) {
  if (checkDiscs(discs, time)) {
    console.log(time);
    break;
  }
  time++;
}

function checkDiscs(discs, time) {
  for (let i = 0; i < discs.length; i++) {
    const disc = discs[i];
    const position = (disc.startPosition + time + i + 1) % disc.totalPositions;
    if (position !== 0) {
      return false;
    }
  }
  return true;
}