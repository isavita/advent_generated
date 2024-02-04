const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');
const player1Start = parseInt(data[0].slice(28).trim());
const player2Start = parseInt(data[1].slice(28).trim());
let player1Pos = player1Start;
let player2Pos = player2Start;

let player1Score = 0;
let player2Score = 0;

let dieRoll = 1;
let rollCount = 0;

while (true) {
    // Player 1
    let rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
    rollCount += 3;
    dieRoll += 3;

    player1Pos = (player1Pos + rolls - 1) % 10 + 1;
    player1Score += player1Pos;

    if (player1Score >= 1000) {
        console.log("Result:", player2Score * rollCount);
        break;
    }

    // Player 2
    rolls = dieRoll % 100 + (dieRoll + 1) % 100 + (dieRoll + 2) % 100;
    rollCount += 3;
    dieRoll += 3;

    player2Pos = (player2Pos + rolls - 1) % 10 + 1;
    player2Score += player2Pos;

    if (player2Score >= 1000) {
        console.log(player1Score * rollCount);
        break;
    }
}