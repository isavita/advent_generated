
import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8').trim();

const disk: string[] = [];
let fileID = 0;
let isFile = true;
for (let i = 0; i < data.length; i++) {
  const length = parseInt(data[i]);
  disk.push(...(isFile ? Array(length).fill(String(fileID++)) : Array(length).fill('.')));
  isFile = !isFile;
}

const files: { id: number; start: number; end: number }[] = [];
let curID = -1;
let start = 0;
for (let i = 0; i < disk.length; i++) {
  if (disk[i] === '.') {
    curID = -1;
    continue;
  }
  const id = parseInt(disk[i]);
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
      spanLen++;
      leftmostSpan = (leftmostSpan === -1) ? j : leftmostSpan;
      if (spanLen === fileLen) break;
    } else {
      spanLen = 0;
      leftmostSpan = -1;
    }
  }

  if (leftmostSpan !== -1 && spanLen === fileLen) {
    disk.splice(f.start, fileLen, ...Array(fileLen).fill('.'));
    disk.splice(leftmostSpan, fileLen, ...Array(fileLen).fill(String(f.id)));
  }
}

let checksum = 0;
for (let i = 0; i < disk.length; i++) {
  if (disk[i] !== '.') {
    checksum += i * parseInt(disk[i]);
  }
}
console.log(checksum);
