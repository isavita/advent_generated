const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let totalCount = 0;
let groupAnswers = {};
let groupSize = 0;

data.forEach((line) => {
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
    for (let i = 0; i < line.length; i++) {
      const question = line[i];
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