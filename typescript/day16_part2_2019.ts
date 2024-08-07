import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8');
const repeatedInput = repeatInput(input, 10000);
const offset = parseInt(input.slice(0, 7), 10);

for (let phase = 0; phase < 100; phase++) {
  let sum = 0;
  for (let i = repeatedInput.length - 1; i >= offset; i--) {
    sum += repeatedInput[i];
    repeatedInput[i] = sum % 10;
  }
}

console.log(repeatedInput.slice(offset, offset + 8).join(''));

function repeatInput(input: string, times: number): number[] {
  const digits: number[] = [];
  for (let t = 0; t < times; t++) {
    for (let i = 0; i < input.length; i++) {
      digits.push(parseInt(input[i], 10));
    }
  }
  return digits;
}