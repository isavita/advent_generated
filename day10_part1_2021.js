const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');
let totalScore = 0;

for (let i = 0; i < data.length; i++) {
    const line = data[i];
    const [score, corrupted] = checkLine(line);
    if (corrupted) {
        totalScore += score;
    }
}

console.log(totalScore);

function checkLine(line) {
    const pairings = {')': '(', ']': '[', '}': '{', '>': '<'};
    const scores = {')': 3, ']': 57, '}': 1197, '>': 25137};
    const stack = [];

    for (let j = 0; j < line.length; j++) {
        const char = line[j];
        switch (char) {
            case '(': case '[': case '{': case '<':
                stack.push(char);
                break;
            case ')': case ']': case '}': case '>':
                if (stack.length === 0 || stack[stack.length - 1] !== pairings[char]) {
                    return [scores[char], true]; // corrupted line
                }
                stack.pop(); // pop from stack
                break;
        }
    }
    return [0, false]; // line is not corrupted
}