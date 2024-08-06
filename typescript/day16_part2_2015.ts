// Replaced import * as fs from 'fs' with const fs = require('fs');
const fs = require('fs');

const targetSue = {
  "children": 3,
  "cats": 7,
  "samoyeds": 2,
  "pomeranians": 3,
  "akitas": 0,
  "vizslas": 0,
  "goldfish": 5,
  "trees": 3,
  "cars": 2,
  "perfumes": 1,
};

function auntSue(input) {
  for (const line of input.split('\n')) {
    const match = line.match(/Sue (\d+): (.+): (\d+), (.+): (\d+), (.+): (\d+)/);
    if (!match) continue;
    const sueNum = parseInt(match[1]);
    const thing1 = match[2].trim();
    const amount1 = parseInt(match[3]);
    const thing2 = match[4].trim();
    const amount2 = parseInt(match[5]);
    const thing3 = match[6].trim();
    const amount3 = parseInt(match[7]);

    const readingsMap = {
      [thing1]: amount1,
      [thing2]: amount2,
      [thing3]: amount3,
    };

    let allRulesMatched = true;
    for (const check of ['cats', 'trees']) {
      if (readingsMap[check] !== undefined && readingsMap[check] <= targetSue[check]) {
        allRulesMatched = false;
      }
      delete readingsMap[check];
    }
    for (const check of ['pomeranians', 'goldfish']) {
      if (readingsMap[check] !== undefined && readingsMap[check] >= targetSue[check]) {
        allRulesMatched = false;
      }
      delete readingsMap[check];
    }

    for (const thing in readingsMap) {
      if (targetSue[thing] !== readingsMap[thing]) {
        allRulesMatched = false;
      }
    }
    if (allRulesMatched) {
      return sueNum;
    }
  }
  throw new Error('expect return from loop');
}

const input = fs.readFileSync('input.txt', 'utf8');
console.log(auntSue(input));