const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const dirSizes = {};

let currentDir = '/';
dirSizes[currentDir] = 0;

for (const line of input) {
  if (line.startsWith('$ cd')) {
    const dir = line.split(' ')[2];
    if (dir === '/') {
      currentDir = '/';
    } else if (dir === '..') {
      currentDir = currentDir.split('/').slice(0, -1).join('/') || '/';
    } else {
      currentDir += `/${dir}`;
      dirSizes[currentDir] = 0;
    }
  } else if (!line.startsWith('$ ls')) {
    const [size, name] = line.split(' ');
    if (size !== 'dir') {
      let dir = currentDir;
      while (dir !== '/') {
        dirSizes[dir] += parseInt(size);
        dir = dir.split('/').slice(0, -1).join('/') || '/';
      }
      dirSizes['/'] += parseInt(size);
    }
  }
}

let sum = 0;
for (const size of Object.values(dirSizes)) {
  if (size <= 100000) {
    sum += size;
  }
}

console.log(sum);