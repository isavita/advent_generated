import * as fs from 'fs';

const input = parseInt(fs.readFileSync('input.txt', 'utf-8').trim(), 10);

function calculateScores(numRecipes: number): string {
    const scores = [3, 7];
    let elf1 = 0;
    let elf2 = 1;

    while (scores.length < numRecipes + 10) {
        const newScore = scores[elf1] + scores[elf2];
        if (newScore >= 10) {
            scores.push(1, newScore % 10);
        } else {
            scores.push(newScore);
        }
        elf1 = (elf1 + scores[elf1] + 1) % scores.length;
        elf2 = (elf2 + scores[elf2] + 1) % scores.length;
    }

    return scores.slice(numRecipes, numRecipes + 10).join('');
}

console.log(calculateScores(input));