const fs = require('fs');

function calculatePoints(winningNumbers, yourNumbers) {
    let points = 0;
    for (const num of yourNumbers) {
        if (winningNumbers.includes(num)) {
            points = points === 0 ? 1 : points * 2;
        }
    }
    return points;
}

function convertToIntArray(str) {
    return str.trim().split(/\s+/).map(Number);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error opening file:", err);
        return;
    }

    const lines = data.split('\n');
    let totalPoints = 0;

    for (const line of lines) {
        if (line.trim().length === 0) continue;
        const parts = line.split(' | ');
        const winningNumbers = convertToIntArray(parts[0]);
        const yourNumbers = convertToIntArray(parts[1]);
        totalPoints += calculatePoints(winningNumbers, yourNumbers);
    }

    console.log(totalPoints);
});