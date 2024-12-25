
import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').split('\n');
const availablePatterns = input[0].trim().split(',').map(s => s.trim());
let totalWays = 0;

for (let i = 2; i < input.length; i++) {
  const design = input[i].trim();
  totalWays += countWays(design, availablePatterns);
}

console.log(totalWays);

function countWays(design: string, patterns: string[]): number {
  const n = design.length;
  const dp = new Array(n + 1).fill(0);
  dp[0] = 1;

  for (let i = 1; i <= n; i++) {
    for (const p of patterns) {
      const lp = p.length;
      if (i >= lp && design.substring(i - lp, i) === p) {
        dp[i] += dp[i - lp];
      }
    }
  }

  return dp[n];
}
