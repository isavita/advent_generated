const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const favoriteNumber = parseInt(input);

const isWall = (x, y) => {
  const result = x * x + 3 * x + 2 * x * y + y + y * y + favoriteNumber;
  const binary = result.toString(2);
  const count = (binary.match(/1/g) || []).length;
  return count % 2 !== 0;
};

const isWithinBounds = (x, y) => x >= 0 && y >= 0;

const bfs = (startX, startY, maxSteps) => {
  const visited = new Set();
  const queue = [[startX, startY, 0]];

  while (queue.length > 0) {
    const [x, y, steps] = queue.shift();

    if (steps > maxSteps) {
      continue;
    }

    if (!isWithinBounds(x, y) || isWall(x, y) || visited.has(`${x},${y}`)) {
      continue;
    }

    visited.add(`${x},${y}`);

    queue.push([x + 1, y, steps + 1]);
    queue.push([x - 1, y, steps + 1]);
    queue.push([x, y + 1, steps + 1]);
    queue.push([x, y - 1, steps + 1]);
  }

  return visited.size;
};

console.log(bfs(1, 1, 50));