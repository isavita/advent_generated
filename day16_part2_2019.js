const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

const repeatInput = (input, times) => {
  const digits = new Array(input.length * times);
  for (let t = 0; t < times; t++) {
    for (let i = 0; i < input.length; i++) {
      digits[t * input.length + i] = parseInt(input[i]);
    }
  }
  return digits;
};

const offset = parseInt(input.slice(0, 7));
const repeatedInput = repeatInput(input, 10000);

for (let phase = 0; phase < 100; phase++) {
  let sum = 0;
  for (let i = repeatedInput.length - 1; i >= offset; i--) {
    sum += repeatedInput[i];
    repeatedInput[i] = sum % 10;
  }
}

let message = '';
for (let i = offset; i < offset + 8; i++) {
  message += repeatedInput[i];
}

console.log(message);