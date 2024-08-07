import * as fs from 'fs';
import * as crypto from 'crypto';

// Function to compute MD5 hash
function md5Hash(input: string): string {
    return crypto.createHash('md5').update(input).digest('hex');
}

// Read the secret key from input.txt
const secretKey = fs.readFileSync('input.txt', 'utf8').trim();

// Function to find the lowest positive number that produces a hash starting with five zeroes
function findLowestNumber(secretKey: string): number {
    let number = 0;
    let hash = '';

    while (!hash.startsWith('00000')) {
        number++;
        hash = md5Hash(secretKey + number);
    }

    return number;
}

// Find and print the result
const result = findLowestNumber(secretKey);
console.log(result);