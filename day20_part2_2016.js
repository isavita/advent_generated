const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.split('-').map(Number));

const ranges = input.sort((a, b) => a[0] - b[0]);

let currentMin = 0;
let allowedIPs = 0;

for (const range of ranges) {
  if (currentMin < range[0]) {
    allowedIPs += range[0] - currentMin;
  }
  currentMin = Math.max(currentMin, range[1] + 1);
}

console.log(currentMin, allowedIPs);