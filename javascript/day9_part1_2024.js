
const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf-8').trim();
const disk = [];
let fileID = 0;
let isFile = true;

for (let i = 0; i < data.length; i++) {
  const length = parseInt(data[i]);
  if (isFile) {
    for (let j = 0; j < length; j++) {
      disk.push(fileID);
    }
    fileID++;
  } else {
    for (let j = 0; j < length; j++) {
      disk.push(-1);
    }
  }
  isFile = !isFile;
}

let lfree = -1;
let rfile = -1;
while (true) {
    lfree = disk.findIndex(x => x === -1);
    if (lfree === -1) break;
    for (rfile = disk.length - 1; rfile > lfree; rfile--) {
        if (disk[rfile] !== -1) break;
    }
    if (rfile === lfree) break;
    if (rfile === -1) break;
    disk[lfree] = disk[rfile];
    disk[rfile] = -1;
}

let checksum = 0;
for (let i = 0; i < disk.length; i++) {
  if (disk[i] !== -1) {
    checksum += i * disk[i];
  }
}

console.log(checksum);
