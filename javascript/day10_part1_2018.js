const fs = require('fs');

class Star {
  constructor(x, y, vX, vY) {
    this.x = x;
    this.y = y;
    this.vX = vX;
    this.vY = vY;
    this.next = null;
  }
}

function toInt(s) {
  return parseInt(s, 10);
}

function main() {
  const input = fs.readFileSync('input.txt', 'utf8');
  const lines = input.trim().split('\n');
  const regex = /position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>/;

  let head = new Star(0, 0, 0, 0);
  let tail = head;

  for (const line of lines) {
    const match = regex.exec(line);
    if (match) {
      const star = new Star(
        toInt(match[1]),
        toInt(match[2]),
        toInt(match[3]),
        toInt(match[4])
      );
      tail.next = star;
      tail = star;
    }
  }

  let smallestT = 0;
  let smallestArea = Number.MAX_SAFE_INTEGER;

  for (let t = 1; t < 100000; t++) {
    let maxX = -Infinity;
    let maxY = -Infinity;
    let minX = Infinity;
    let minY = Infinity;

    for (let temp = head.next; temp.next !== null; temp = temp.next) {
      const x = temp.x + temp.vX * t;
      const y = temp.y + temp.vY * t;
      maxX = Math.max(maxX, x);
      minX = Math.min(minX, x);
      maxY = Math.max(maxY, y);
      minY = Math.min(minY, y);
    }

    const lenX = maxX - minX + 1;
    const lenY = maxY - minY + 1;
    const area = lenX + lenY;

    if (area < smallestArea) {
      smallestArea = area;
      smallestT = t;
    }
  }

  const t = smallestT;

  maxX = -Infinity;
  maxY = -Infinity;
  minX = Infinity;
  minY = Infinity;

  for (let temp = head.next; temp.next !== null; temp = temp.next) {
    temp.x = temp.x + temp.vX * t;
    temp.y = temp.y + temp.vY * t;
    maxX = Math.max(maxX, temp.x);
    minX = Math.min(minX, temp.x);
    maxY = Math.max(maxY, temp.y);
    minY = Math.min(minY, temp.y);
  }

  const mapper = Array.from({ length: maxY - minY + 1 }, () =>
    Array(maxX - minX + 1).fill(false)
  );

  for (let temp = head.next; temp.next !== null; temp = temp.next) {
    mapper[temp.y - minY][temp.x - minX] = true;
  }

  for (let i = 0; i < mapper.length; i++) {
    let row = '';
    for (let j = 0; j < mapper[i].length; j++) {
      row += mapper[i][j] ? '#' : ' ';
    }
    console.log(row);
  }
}

main();