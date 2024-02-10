const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

let scores = [];

for (let line of input) {
    const [score, incomplete] = checkAndCompleteLine(line);
    if (incomplete) {
        scores.push(score);
    }
}

scores.sort((a, b) => a - b);
const middleScore = scores[Math.floor(scores.length / 2)];
console.log(middleScore);

function checkAndCompleteLine(line) {
    const pairings = { ')': '(', ']': '[', '}': '{', '>': '<' };
    const scoreValues = { ')': 1, ']': 2, '}': 3, '>': 4 };
    const opening = "([{<";
    const closing = ")]}>";
    let stack = [];

    for (let char of line) {
        if (opening.includes(char)) {
            stack.push(char);
        } else if (closing.includes(char)) {
            if (stack.length === 0 || stack[stack.length - 1] !== pairings[char]) {
                return [0, false];
            }
            stack.pop();
        }
    }

    if (stack.length === 0) {
        return [0, false];
    }

    let score = 0;
    for (let i = stack.length - 1; i >= 0; i--) {
        score *= 5;
        score += scoreValues[getClosingChar(stack[i])];
    }
    return [score, true];
}

function getClosingChar(openingChar) {
    switch (openingChar) {
        case '(':
            return ')';
        case '[':
            return ']';
        case '{':
            return '}';
        case '<':
            return '>';
        default:
            return ' ';
    }
}