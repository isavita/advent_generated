
const fs = require('node:fs');

function calculateMulSum(input) {
  const regex = /mul\(\s*(\d+)\s*,\s*(\d+)\s*\)/g;
  let match;
  let sum = 0;

  while ((match = regex.exec(input)) !== null) {
    sum += parseInt(match[1]) * parseInt(match[2]);
  }

  return sum;
}


fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error("Failed to read the file:", err);
    return;
  }
  const result = calculateMulSum(data);
  console.log("Sum of multiplication results:", result);
});

