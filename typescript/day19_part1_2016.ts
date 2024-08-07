import * as fs from 'fs';

function findWinningElf(numElves: number): number {
    let winner = 0; // Josephus position starts at 0 for 1 elf
    
    for (let i = 1; i <= numElves; i++) {
        winner = (winner + 2) % i; // The step is 2 since every second elf is eliminated
    }

    return winner + 1; // Convert from 0-indexed to 1-indexed
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const numElves = parseInt(input, 10);
    const winner = findWinningElf(numElves);
    console.log(winner);
}

main();