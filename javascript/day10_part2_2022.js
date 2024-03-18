const fs = require('fs');

function readAll(path) {
  return fs.readFileSync(path, 'utf8');
}

function abs(x) {
  return x < 0 ? -x : x;
}

function main() {
  const x = [1];
  const lines = readAll('input.txt').split('\n');
  for (const line of lines) {
    switch (line) {
      case 'noop':
        x.push(x[x.length - 1]);
        break;
      default:
        const n = parseInt(line.split(' ')[1], 10);
        x.push(x[x.length - 1]);
        x.push(x[x.length - 1] + n);
        break;
    }
  }

  const grid = new Set();
  for (let i = 0; i < x.length; i++) {
    const crtx = i % 40;
    const crty = Math.floor(i / 40);
    if (abs(crtx - x[i]) <= 1) {
      grid.add(`${crtx},${crty}`);
    } else {
      grid.delete(`${crtx},${crty}`);
    }
  }

  for (let y = 0; y < 6; y++) {
    let row = '';
    for (let x = 0; x < 40; x++) {
      row += grid.has(`${x},${y}`) ? '#' : '.';
    }
    console.log(row);
  }
}

main();