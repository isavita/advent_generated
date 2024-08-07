import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf8');
let digits = input.split('').map(Number);

for (let phase = 0; phase < 100; phase++) {
  digits = applyFFT(digits);
}

console.log(digits.slice(0, 8).join(''));

function applyFFT(input: number[]): number[] {
  const basePattern = [0, 1, 0, -1];
  const output = new Array(input.length);
  for (let i = 0; i < input.length; i++) {
    let sum = 0;
    for (let j = 0; j < input.length; j++) {
      const patternValue = basePattern[Math.floor((j + 1) / (i + 1)) % basePattern.length];
      sum += input[j] * patternValue;
    }
    output[i] = Math.abs(sum % 10);
  }
  return output;
}