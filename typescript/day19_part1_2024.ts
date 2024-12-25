
import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').split('\n');
const availablePatterns = input[0].trim().split(',').map(s => s.trim());
let count = 0;

for (let i = 2; i < input.length; i++) {
  const design = input[i].trim();
  if (canMake(design, availablePatterns)) {
    count++;
  }
}

console.log(count);

function canMake(design: string, patterns: string[]): boolean {
  const n = design.length;
  const dp = new Array(n + 1).fill(false);
  dp[0] = true;

  const patternSet = new Set(patterns);

  for (let i = 1; i <= n; i++) {
    for (let j = 1; j <= i; j++) {
      if (dp[i - j] && patternSet.has(design.substring(i - j, i))) {
        dp[i] = true;
        break;
      }
    }
  }
  return dp[n];
}
