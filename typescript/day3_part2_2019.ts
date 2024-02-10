const fs = require('fs');

class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
}

function getPointsWithSteps(path) {
  const points = new Map();
  let current = new Point(0, 0);
  let steps = 0;
  path.split(',').forEach(move => {
    const dir = move[0];
    const dist = parseInt(move.slice(1));
    for (let i = 0; i < dist; i++) {
      steps++;
      switch (dir) {
        case 'U':
          current.y++;
          break;
        case 'D':
          current.y--;
          break;
        case 'L':
          current.x--;
          break;
        case 'R':
          current.x++;
          break;
      }
      if (!points.has(JSON.stringify(current))) {
        points.set(JSON.stringify(current), steps);
      }
    }
  });
  return points;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  const lines = data.trim().split('\n');
  const wire1 = getPointsWithSteps(lines[0]);
  const wire2 = getPointsWithSteps(lines[1]);

  let minSteps = Number.MAX_SAFE_INTEGER;
  for (const [p, steps1] of wire1.entries()) {
    if (wire2.has(p)) {
      const steps2 = wire2.get(p);
      const totalSteps = steps1 + steps2;
      if (totalSteps < minSteps) {
        minSteps = totalSteps;
      }
    }
  }

  console.log(minSteps);
});