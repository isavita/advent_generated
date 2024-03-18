const fs = require('fs');

const applyInsertion = (polymer, rules) => {
  let newPolymer = '';
  for (let i = 0; i < polymer.length - 1; i++) {
    newPolymer += polymer[i];
    const pair = polymer.slice(i, i + 2);
    if (rules[pair]) {
      newPolymer += rules[pair];
    }
  }
  newPolymer += polymer[polymer.length - 1];
  return newPolymer;
};

const countElements = (polymer) => {
  const counts = {};
  for (const char of polymer) {
    counts[char] = (counts[char] || 0) + 1;
  }
  return counts;
};

const minMax = (counts) => {
  let min = Infinity;
  let max = -Infinity;
  for (const count of Object.values(counts)) {
    if (count < min) {
      min = count;
    }
    if (count > max) {
      max = count;
    }
  }
  return [min, max];
};

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const [initialPolymer, ...ruleLines] = data.trim().split('\n');
  const rules = {};
  for (const line of ruleLines) {
    const [pair, insert] = line.split(' -> ');
    rules[pair] = insert;
  }

  let polymer = initialPolymer;
  for (let step = 0; step < 10; step++) {
    polymer = applyInsertion(polymer, rules);
  }

  const counts = countElements(polymer);
  const [min, max] = minMax(counts);

  console.log(max - min);
});