
const fs = require('node:fs');

function solve() {
  const input = fs.readFileSync('input.txt', 'utf8');
  const re = /(mul\(\d{1,3},\d{1,3}\))|(do\(\))|(don't\(\))/g;
  let enabled = true;
  let totalSum = 0;
  let match;

  while ((match = re.exec(input)) !== null) {
    if (match[1]) {
      if (enabled) {
        const nums = match[1].substring(4, match[1].length - 1).split(',');
        totalSum += parseInt(nums[0]) * parseInt(nums[1]);
      }
    } else if (match[2]) {
      enabled = true;
    } else if (match[3]) {
      enabled = false;
    }
  }
  console.log(totalSum);
}

solve();
