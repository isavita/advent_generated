const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let totalCount = 0;
let groupAnswers = new Set();

for (const line of data) {
    if (line === '') {
        totalCount += groupAnswers.size;
        groupAnswers = new Set();
    } else {
        for (const question of line) {
            groupAnswers.add(question);
        }
    }
}

totalCount += groupAnswers.size;
console.log(totalCount);