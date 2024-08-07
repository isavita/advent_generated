import * as fs from 'fs';
import * as crypto from 'crypto';

// Function to calculate MD5 hash
function calculateMD5(input: string): string {
  return crypto.createHash('md5').update(input).digest('hex');
}

// Function to find the lowest positive number that produces a hash starting with the given number of zeroes
function findLowestNumber(secretKey: string, zeroes: number): number {
  let number = 0;
  const target = '0'.repeat(zeroes);

  while (true) {
    const hash = calculateMD5(secretKey + number);
    if (hash.startsWith(target)) {
      return number;
    }
    number++;
  }
}

// Read the secret key from input.txt
const secretKey = fs.readFileSync('input.txt', 'utf-8').trim();

// Part One: Find the lowest number that produces a hash starting with five zeroes
const partOneResult = findLowestNumber(secretKey, 5);
console.log(`Part One: ${partOneResult}`);

// Part Two: Find the lowest number that produces a hash starting with six zeroes
const partTwoResult = findLowestNumber(secretKey, 6);
console.log(`Part Two: ${partTwoResult}`);