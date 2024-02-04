const fs = require('fs');

const invalidNumber = 14360655;

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

for (let i = 0; i < input.length; i++) {
  let sum = input[i];
  let min = input[i];
  let max = input[i];
  for (let j = i + 1; j < input.length; j++) {
    sum += input[j];
    if (input[j] < min) {
      min = input[j];
    }
    if (input[j] > max) {
      max = input[j];
    }
    if (sum === invalidNumber) {
      console.log(min + max);
      process.exit();
    } else if (sum > invalidNumber) {
      break;
    }
  }
}