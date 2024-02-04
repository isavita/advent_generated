const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf8');
const lines = file.split('\n');

let totalCount = 0;
let groupAnswers = {};

for (const line of lines) {
    if (line === '') {
        totalCount += Object.keys(groupAnswers).length;
        groupAnswers = {};
    } else {
        for (const question of line) {
            groupAnswers[question] = true;
        }
    }
}

totalCount += Object.keys(groupAnswers).length;
console.log(totalCount);