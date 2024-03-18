const fs = require('fs');

const readInput = (filename) => {
  const data = fs.readFileSync(filename, 'utf8');
  const [template, rulesData] = data.split('\n\n');
  const rules = new Map();
  for (const line of rulesData.split('\n')) {
    if (line.trim() === '') continue;
    const [pair, insert] = line.split(' -> ');
    rules.set(pair, insert);
  }
  return [template, rules];
};

const solve = (template, rules) => {
  const pairCounts = new Map();
  for (let i = 0; i < template.length - 1; i++) {
    const pair = template.slice(i, i + 2);
    pairCounts.set(pair, (pairCounts.get(pair) || 0) + 1);
  }

  for (let step = 0; step < 40; step++) {
    const newPairCounts = new Map();
    for (const [pair, count] of pairCounts) {
      const insert = rules.get(pair);
      if (insert) {
        newPairCounts.set(pair.slice(0, 1) + insert, (newPairCounts.get(pair.slice(0, 1) + insert) || 0) + count);
        newPairCounts.set(insert + pair.slice(1), (newPairCounts.get(insert + pair.slice(1)) || 0) + count);
      } else {
        newPairCounts.set(pair, (newPairCounts.get(pair) || 0) + count);
      }
    }
    pairCounts.clear();
    for (const [pair, count] of newPairCounts) {
      pairCounts.set(pair, count);
    }
  }

  const elementCounts = new Map();
  for (const [pair, count] of pairCounts) {
    elementCounts.set(pair[0], (elementCounts.get(pair[0]) || 0) + count);
  }
  elementCounts.set(template[template.length - 1], (elementCounts.get(template[template.length - 1]) || 0) + 1);

  let maxCount = 0, minCount = Infinity;
  for (const count of elementCounts.values()) {
    if (count > maxCount) maxCount = count;
    if (count < minCount) minCount = count;
  }

  console.log(maxCount - minCount);
};

const [template, rules] = readInput('input.txt');
solve(template, rules);