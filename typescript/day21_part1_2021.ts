const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const lines = data.split('\n');
const player1Start = parseInt(lines[0].trim().slice(28));
const player2Start = parseInt(lines[1].trim().slice(28));

let player1Pos = player1Start;
let player2Pos = player2Start;
let player1Score = 0;
let player2Score = 0;
let dieRoll = 1;
let rollCount = 0;

while (true) {
  const rolls = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100);
  rollCount += 3;
  dieRoll += 3;

  player1Pos = ((player1Pos + rolls - 1) % 10) + 1;
  player1Score += player1Pos;

  if (player1Score >= 1000) {
    console.log(player2Score * rollCount);
    break;
  }

  const rolls2 = (dieRoll % 100) + ((dieRoll + 1) % 100) + ((dieRoll + 2) % 100);
  rollCount += 3;
  dieRoll += 3;

  player2Pos = ((player2Pos + rolls2 - 1) % 10) + 1;
  player2Score += player2Pos;

  if (player2Score >= 1000) {
    console.log(player1Score * rollCount);
    break;
  }
}