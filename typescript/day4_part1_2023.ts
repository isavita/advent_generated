import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
let totalPoints = 0;

input.forEach(line => {
    const [winningStr, yourStr] = line.split(' | ');
    const winningNumbers = convertToIntArray(winningStr);
    const yourNumbers = convertToIntArray(yourStr);
    totalPoints += calculatePoints(winningNumbers, yourNumbers);
});

console.log(totalPoints);

function convertToIntArray(str: string): number[] {
    return str.split(/\s+/).map(Number);
}

function calculatePoints(winningNumbers: number[], yourNumbers: number[]): number {
    let points = 0;
    yourNumbers.forEach(num => {
        if (winningNumbers.includes(num)) {
            if (points === 0) {
                points = 1;
            } else {
                points *= 2;
            }
        }
    });
    return points;
}