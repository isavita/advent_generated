const fs = require('fs');

class Point {
  constructor(x, y) {
    this.x = x;
    this.y = y;
  }
}

function main() {
  const input = fs.readFileSync('input.txt').toString().split('\n');

  let points = new Map();
  let folds = [];
  let readingPoints = true;

  for (let line of input) {
    if (line === '') {
      readingPoints = false;
      continue;
    }

    if (readingPoints) {
      let coords = line.split(',');
      let x = parseInt(coords[0]);
      let y = parseInt(coords[1]);
      points.set(JSON.stringify(new Point(x, y)), true);
    } else {
      folds.push(line);
    }
  }

  let fold = folds[0].split(' ')[2];
  let axisValue = fold.split('=')[1];
  let axis = fold.split('=')[0];
  let value = parseInt(axisValue);

  let newPoints = new Map();
  if (axis === 'x') {
    for (let [point, _] of points) {
      point = JSON.parse(point);
      if (point.x > value) {
        point.x = 2 * value - point.x;
      }
      newPoints.set(JSON.stringify(point), true);
    }
  } else {
    for (let [point, _] of points) {
      point = JSON.parse(point);
      if (point.y > value) {
        point.y = 2 * value - point.y;
      }
      newPoints.set(JSON.stringify(point), true);
    }
  }

  console.log(newPoints.size);
}

main();