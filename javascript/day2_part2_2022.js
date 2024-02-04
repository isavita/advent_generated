const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let totalScore = 0;

data.forEach(line => {
    const opponent = line[0];
    const roundEnd = line[2];

    let yourMove = ' ';
    if (roundEnd === 'X') {
        yourMove = opponent === 'A' ? 'Z' : opponent === 'B' ? 'X' : 'Y';
    } else if (roundEnd === 'Y') {
        yourMove = opponent === 'A' ? 'X' : opponent === 'B' ? 'Y' : 'Z';
    } else {
        yourMove = opponent === 'A' ? 'Y' : opponent === 'B' ? 'Z' : 'X';
    }

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
});

console.log(totalScore);