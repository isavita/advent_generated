const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => line.split('\t').map(Number));

const checksum = input.reduce((sum, row) => sum + (Math.max(...row) - Math.min(...row)), 0);
console.log(checksum);

const divisibleSum = input.reduce((sum, row) => {
  for (let i = 0; i < row.length; i++) {
    for (let j = i + 1; j < row.length; j++) {
      if (row[i] % row[j] === 0) {
        return sum + row[i] / row[j];
      }
      if (row[j] % row[i] === 0) {
        return sum + row[j] / row[i];
      }
    }
  }
  return sum;
}, 0);

console.log(divisibleSum);