import * as fs from 'fs';

const errorScores: Record<string, number> = {
    ')': 3,
    ']': 57,
    '}': 1197,
    '>': 25137
};

const matchingBrackets: Record<string, string> = {
    '(': ')',
    '[': ']',
    '{': '}',
    '<': '>'
};

const calculateSyntaxErrorScore = (lines: string[]): number => {
    let totalScore = 0;

    for (const line of lines) {
        const stack: string[] = [];
        let corruptedChar: string | null = null;

        for (const char of line) {
            if (matchingBrackets[char]) {
                stack.push(char);
            } else {
                const lastOpen = stack.pop();
                if (lastOpen && matchingBrackets[lastOpen] !== char) {
                    corruptedChar = char;
                    break;
                }
            }
        }

        if (corruptedChar) {
            totalScore += errorScores[corruptedChar];
        }
    }

    return totalScore;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const lines = input.trim().split('\n');
    const totalScore = calculateSyntaxErrorScore(lines);
    console.log(totalScore);
};

main();