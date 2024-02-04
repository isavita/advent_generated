const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();
const inputNumber = parseInt(input);

let scoreboard = [3, 7];
let elf1 = 0;
let elf2 = 1;

while (scoreboard.length < inputNumber + 10) {
    const newScore = scoreboard[elf1] + scoreboard[elf2];
    if (newScore >= 10) {
        scoreboard.push(Math.floor(newScore / 10));
    }
    scoreboard.push(newScore % 10);

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length;
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length;
}

let answer = '';
for (let i = inputNumber; i < inputNumber + 10; i++) {
    answer += scoreboard[i];
}

console.log(answer);