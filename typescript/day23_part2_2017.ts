const fs = require('fs');

function isPrime(n: number): boolean {
  for (let i = 2; i * i <= n; i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
}

const input = fs.readFileSync('input.txt', 'utf8');
const b = 57 * 100 + 100000;
const c = b + 17000;
let h = 0;

for (let x = b; x <= c; x += 17) {
  if (!isPrime(x)) {
    h++;
  }
}

console.log(h);