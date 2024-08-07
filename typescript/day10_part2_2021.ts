import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

const openChars = new Map<string, string>([
    ['(', ')'],
    ['[', ']'],
    ['{', '}'],
    ['<', '>']
]);

const errorPoints = new Map<string, number>([
    [')', 3],
    [']', 57],
    ['}', 1197],
    ['>', 25137]
]);

const autocompletePoints = new Map<string, number>([
    [')', 1],
    [']', 2],
    ['}', 3],
    ['>', 4]
]);

function isCorrupted(line: string): [boolean, string | null] {
    const stack: string[] = [];
    for (const char of line) {
        if (openChars.has(char)) {
            stack.push(openChars.get(char)!);
        } else {
            const expected = stack.pop();
            if (expected !== char) {
                return [true, char]; // Found a corruption
            }
        }
    }
    return [false, null]; // Not corrupted
}

function autocomplete(line: string): string {
    const stack: string[] = [];
    for (const char of line) {
        if (openChars.has(char)) {
            stack.push(openChars.get(char)!);
        } else {
            stack.pop();
        }
    }
    return stack.reverse().join('');
}

let totalSyntaxErrorScore = 0;
const autocompleteScores: number[] = [];

for (const line of input) {
    const [corrupted, char] = isCorrupted(line);
    if (corrupted) {
        totalSyntaxErrorScore += errorPoints.get(char!)!;
    } else {
        const completionString = autocomplete(line);
        let score = 0;
        for (const char of completionString) {
            score = score * 5 + autocompletePoints.get(char)!;
        }
        autocompleteScores.push(score);
    }
}

console.log(`Total Syntax Error Score: ${totalSyntaxErrorScore}`);

autocompleteScores.sort((a, b) => a - b);
const middleScore = autocompleteScores[Math.floor(autocompleteScores.length / 2)];
console.log(`Middle Autocomplete Score: ${middleScore}`);