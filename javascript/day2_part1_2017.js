const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(row => row.split('\t').map(Number));

const checksum = input.reduce((sum, row) => sum + (Math.max(...row) - Math.min(...row)), 0);

console.log(checksum);