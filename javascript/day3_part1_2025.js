
const fs = require('fs');

function maxJoltage(s) {
  for (let d1 = 9; d1 >= 0; d1--) {
    const ch = String.fromCharCode(48 + d1);
    const i = s.indexOf(ch);
    if (i !== -1 && i < s.length - 1) {
      let maxD2 = -1;
      for (let j = i + 1; j < s.length; j++) {
        const code = s.charCodeAt(j);
        if (code >= 48 && code <= 57) {
          const d2 = code - 48;
          if (d2 > maxD2) maxD2 = d2;
        }
      }
      if (maxD2 !== -1) return d1 * 10 + maxD2;
    }
  }
  return 0;
}

(function main() {
  const total = fs.readFileSync('input.txt', 'utf8')
    .split('\n')
    .map(l => l.trim())
    .filter(l => l)
    .reduce((sum, line) => sum + maxJoltage(line), 0);
  console.log(`Total output joltage: ${total}`);
})();
