const fs = require('fs');

const Neighbors4 = [[0, 1], [0, -1], [1, 0], [-1, 0]];

const grid = {};
let y = 0;

const data = fs.readFileSync('input.txt', 'utf8').split('\n');
data.forEach((line) => {
  [...line].forEach((b, x) => {
    grid[`${x},${y}`] = parseInt(b);
  });
  y++;
});

let maxScore = 0;
Object.keys(grid).forEach((key) => {
  const p = key.split(',').map(Number);
  let score = 1;
  Neighbors4.forEach((n) => {
    let [dx, dy] = n;
    let [px, py] = p;
    let view = 0;
    while (true) {
      px += dx;
      py += dy;
      if (grid[`${px},${py}`] !== undefined) {
        view++;
        if (grid[`${px},${py}`] >= grid[`${p[0]},${p[1]}`]) {
          score *= view;
          break;
        }
      } else {
        score *= view;
        break;
      }
    }
  });

  if (score > maxScore) {
    maxScore = score;
  }
});

console.log(maxScore);