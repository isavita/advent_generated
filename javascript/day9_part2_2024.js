
const fs = require('fs');

const solve = () => {
  const line = fs.readFileSync('input.txt', 'utf-8').trim();
  let disk = [];
  let fileID = 0;
  let isFile = true;

  for (let i = 0; i < line.length; i++) {
    const length = parseInt(line[i]);
    if (isFile) {
      for (let j = 0; j < length; j++) {
        disk.push(String(fileID));
      }
      fileID++;
    } else {
      for (let j = 0; j < length; j++) {
        disk.push('.');
      }
    }
    isFile = !isFile;
  }

  const files = [];
  let curID = -1;
  let start = 0;
  for (let i = 0; i < disk.length; i++) {
    const b = disk[i];
    if (b === '.') {
      curID = -1;
      continue;
    }
    const id = parseInt(b);
    if (id !== curID) {
      curID = id;
      start = i;
    }
    if (i === disk.length - 1 || (i + 1 < disk.length && parseInt(disk[i + 1]) !== id)) {
      files.push({ id, start, end: i });
    }
  }

  for (let i = files.length - 1; i >= 0; i--) {
    const f = files[i];
    const fileLen = f.end - f.start + 1;
    let leftmostSpan = -1;
    let spanLen = 0;
    for (let j = 0; j < f.start; j++) {
      if (disk[j] === '.') {
        if (spanLen === 0) {
          leftmostSpan = j;
        }
        spanLen++;
        if (spanLen === fileLen) {
          break;
        }
      } else {
        spanLen = 0;
        leftmostSpan = -1;
      }
    }
    if (leftmostSpan !== -1 && spanLen === fileLen) {
      for (let x = f.start; x <= f.end; x++) {
        disk[x] = '.';
      }
      for (let x = 0; x < fileLen; x++) {
        disk[leftmostSpan + x] = String(f.id);
      }
    }
  }

  let checksum = 0;
  for (let i = 0; i < disk.length; i++) {
    const b = disk[i];
    if (b !== '.') {
      const id = parseInt(b);
      checksum += i * id;
    }
  }
  console.log(checksum);
};

solve();
