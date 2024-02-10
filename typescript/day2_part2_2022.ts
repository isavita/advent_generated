const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let totalScore = 0;

for (let i = 0; i < data.length; i++) {
    const line = data[i];
    const opponent = line[0];
    const roundEnd = line[2];

    let yourMove = ' ';
    if (roundEnd === 'X') {
        if (opponent === 'A') {
            yourMove = 'Z';
        } else if (opponent === 'B') {
            yourMove = 'X';
        } else {
            yourMove = 'Y';
        }
    } else if (roundEnd === 'Y') {
        if (opponent === 'A') {
            yourMove = 'X';
        } else if (opponent === 'B') {
            yourMove = 'Y';
        } else {
            yourMove = 'Z';
        }
    } else {
        if (opponent === 'A') {
            yourMove = 'Y';
        } else if (opponent === 'B') {
            yourMove = 'Z';
        } else {
            yourMove = 'X';
        }
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
}

console.log(totalScore);