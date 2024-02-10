const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.trim().split('\n');

let totalCount = 0;
let groupAnswers = {};
let groupSize = 0;

lines.forEach((line) => {
  if (line === '') {
    Object.values(groupAnswers).forEach((count) => {
      if (count === groupSize) {
        totalCount++;
      }
    });
    groupAnswers = {};
    groupSize = 0;
  } else {
    groupSize++;
    for (const question of line) {
      groupAnswers[question] = (groupAnswers[question] || 0) + 1;
    }
  }
});

Object.values(groupAnswers).forEach((count) => {
  if (count === groupSize) {
    totalCount++;
  }
});

console.log(totalCount);