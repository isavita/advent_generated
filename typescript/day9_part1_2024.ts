
import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8').trim();
const disk: string[] = [];
let fileID = 0;
let isFile = true;
for (let i = 0; i < data.length; i++) {
  const length = parseInt(data[i]);
  if (isFile) {
    disk.push(...Array(length).fill(String(fileID)));
    fileID++;
  } else {
    disk.push(...Array(length).fill('.'));
  }
  isFile = !isFile;
}

while (true) {
  let lfree = -1;
  for (let i = 0; i < disk.length; i++) {
    if (disk[i] === '.') {
      lfree = i;
      break;
    }
  }
  if (lfree === -1) break;
  let rfile = -1;
  for (let i = disk.length - 1; i > lfree; i--) {
    if (disk[i] !== '.') {
      rfile = i;
      break;
    }
  }
  if (rfile === -1) break;
  disk[lfree] = disk[rfile];
  disk[rfile] = '.';
}

let checksum = 0;
for (let i = 0; i < disk.length; i++) {
  if (disk[i] !== '.') {
    const id = parseInt(disk[i]);
    checksum += i * id;
  }
}
console.log(checksum);
