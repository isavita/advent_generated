const fs = require('fs');

function main() {
  const inputStr = fs.readFileSync('input.txt', 'utf8').trim();
  const lines = inputStr.split(/\n/);

  const ground = [['+']]
  let maxX = 0, minX = 0, maxY = 0, minY = 20;
  const xOffset = 500, yOffset = 0;

  for (const line of lines) {
    const split = line.split(/[=, .]+/);
    if (split[0] === 'x') {
      const x = parseInt(split[1]) - xOffset;
      const y1 = parseInt(split[3]) - yOffset;
      const y2 = parseInt(split[4]) - yOffset;

      while (x >= maxX) {
        maxX++;
        for (let j = 0; j < ground.length; j++) {
          ground[j].push('.');
        }
      }
      while (x <= minX) {
        minX--;
        for (let j = 0; j < ground.length; j++) {
          ground[j].unshift('.');
        }
      }
      while (y2 > maxY) {
        maxY++;
        ground.push(Array(ground[0].length).fill('.'));
      }
      if (y1 < minY) {
        minY = y1;
      }
      for (let i = y1; i <= y2; i++) {
        ground[i][x - minX] = '#';
      }
    } else {
      const y = parseInt(split[1]) - yOffset;
      const x1 = parseInt(split[3]) - xOffset;
      const x2 = parseInt(split[4]) - xOffset;

      while (y > maxY) {
        maxY++;
        ground.push(Array(ground[0].length).fill('.'));
      }
      while (x2 >= maxX) {
        maxX++;
        for (let j = 0; j < ground.length; j++) {
          ground[j].push('.');
        }
      }
      while (x1 <= minX) {
        minX--;
        for (let j = 0; j < ground.length; j++) {
          ground[j].unshift('.');
        }
      }
      for (let i = x1; i <= x2; i++) {
        ground[y][i - minX] = '#';
      }
      if (y < minY) {
        minY = y;
      }
    }
  }

  let waterCount = 0;
  let flowCount = 0;
  const roundLimit = 200000;

  while (ground[1][-minX] !== '|' && waterCount < roundLimit) {
    let canMove = true;
    let x = -minX;
    let y = 1;
    let tryLeft = 0;
    while (canMove) {
      if (y + 1 > maxY || ground[y + 1][x] === '|') {
        ground[y][x] = '|';
        canMove = false;
        if (y >= minY) {
          flowCount++;
        }
      } else if (ground[y + 1][x] === '.') {
        y++;
        tryLeft = 0;
      } else if (ground[y + 1][x] === '#' || ground[y + 1][x] === '~') {
        if ((tryLeft === 1 && ground[y][x - 1] === '|') ||
            (tryLeft === 2 && ground[y][x + 1] === '|') ||
            (ground[y][x + 1] === '|' && ground[y][x - 1] !== '.') ||
            (ground[y][x + 1] !== '.' && ground[y][x - 1] === '|')) {
          ground[y][x] = '|';
          flowCount++;
          canMove = false;
          for (let i = x + 1; ground[y][i] === '~'; i++) {
            ground[y][i] = '|';
            waterCount--;
            flowCount++;
          }
          for (let i = x - 1; ground[y][i] === '~'; i--) {
            ground[y][i] = '|';
            waterCount--;
            flowCount++;
          }
        } else if ((tryLeft === 0 && ground[y][x - 1] === '.') ||
                   (tryLeft === 1 && ground[y][x - 1] === '.')) {
          x--;
          tryLeft = 1;
        } else if ((tryLeft === 0 && ground[y][x + 1] === '.') ||
                   (tryLeft === 2 && ground[y][x + 1] === '.')) {
          x++;
          tryLeft = 2;
        } else {
          canMove = false;
          ground[y][x] = '~';
          waterCount++;
        }
      }
    }
  }

  console.log(flowCount + waterCount);
}

main();