
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(Number);

for (let i = 0; i < input.length - 1; i++) {
  for (let j = i + 1; j < input.length; j++) {
    if (input[i] + input[j] === 2020) {
      console.log(input[i] * input[j]);
      process.exit();
    }
  }
}
