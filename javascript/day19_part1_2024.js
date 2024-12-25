
const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
const availablePatterns = input[0].split(',').map(p => p.trim());
let count = 0;

for (let i = 2; i < input.length; i++) {
    const design = input[i];
    if (canMake(design, availablePatterns)) {
        count++;
    }
}

console.log(count);

function canMake(design, patterns) {
    const n = design.length;
    const dp = new Array(n + 1).fill(false);
    dp[0] = true;
    for (let i = 1; i <= n; i++) {
        for (const p of patterns) {
            const lp = p.length;
            if (i >= lp && dp[i - lp] && design.substring(i - lp, i) === p) {
                dp[i] = true;
                break;
            }
        }
    }
    return dp[n];
}
