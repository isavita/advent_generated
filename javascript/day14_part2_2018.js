const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim();

let scoreboard = [3, 7];
let elf1 = 0;
let elf2 = 1;
const inputLen = input.length;
const inputSequence = input.split('').map(Number);

while (true) {
    const newScore = scoreboard[elf1] + scoreboard[elf2];
    if (newScore >= 10) {
        scoreboard.push(Math.floor(newScore / 10));
        if (checkSequence(scoreboard, inputSequence)) {
            break;
        }
    }
    scoreboard.push(newScore % 10);
    if (checkSequence(scoreboard, inputSequence)) {
        break;
    }

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length;
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length;
}

console.log(scoreboard.length - inputLen);

function checkSequence(scoreboard, sequence) {
    if (scoreboard.length < sequence.length) {
        return false;
    }
    const start = scoreboard.length - sequence.length;
    for (let i = 0; i < sequence.length; i++) {
        if (scoreboard[start + i] !== sequence[i]) {
            return false;
        }
    }
    return true;
}