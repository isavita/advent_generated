const fs = require('fs');

const fileContent = fs.readFileSync('input.txt', 'utf8');
const lines = fileContent.split('\n');

let sum = 0;

lines.forEach((line) => {
  if (line === '') {
    return;
  }

  let firstDigit = -1;
  let lastDigit = -1;

  for (let i = 0; i < line.length; i++) {
    const char = line.charAt(i);
    if (!isNaN(parseInt(char))) {
      if (firstDigit === -1) {
        firstDigit = parseInt(char);
      }
      lastDigit = parseInt(char);
    }
  }

  if (firstDigit !== -1 && lastDigit !== -1) {
    const value = parseInt(`${firstDigit}${lastDigit}`);
    sum += value;
  }
});

console.log(sum);