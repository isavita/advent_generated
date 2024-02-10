const fs = require('fs');

const input = parseInt(fs.readFileSync('input.txt', 'utf8').trim());

let scoreboard = [3, 7];
let elf1 = 0;
let elf2 = 1;

while (scoreboard.length < input + 10) {
    const newScore = scoreboard[elf1] + scoreboard[elf2];
    if (newScore >= 10) {
        scoreboard.push(Math.floor(newScore / 10));
    }
    scoreboard.push(newScore % 10);

    elf1 = (elf1 + scoreboard[elf1] + 1) % scoreboard.length;
    elf2 = (elf2 + scoreboard[elf2] + 1) % scoreboard.length;
}

for (let i = input; i < input + 10; i++) {
    process.stdout.write(scoreboard[i].toString());
}
console.log();