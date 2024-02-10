
const fs = require('fs');

const file = fs.readFileSync('input.txt', 'utf-8');
const lines = file.split('\n');

let totalScore = 0;

for (const line of lines) {
    const opponent = line[0];
    const yourMove = line[2];

    let score = 0;
    if (yourMove === 'X') {
        score = 1;
    } else if (yourMove === 'Y') {
        score = 2;
    } else if (yourMove === 'Z') {
        score = 3;
    }

    if ((opponent === 'A' && yourMove === 'Y') || (opponent === 'B' && yourMove === 'Z') || (opponent === 'C' && yourMove === 'X')) {
        score += 6;
    } else if ((opponent === 'A' && yourMove === 'X') || (opponent === 'B' && yourMove === 'Y') || (opponent === 'C' && yourMove === 'Z')) {
        score += 3;
    }

    totalScore += score;
}

console.log(totalScore);
