const fs = require('fs');

function countCombinations(containers, target, index) {
    if (target === 0) {
        return 1;
    }
    if (target < 0 || index >= containers.length) {
        return 0;
    }
    return countCombinations(containers, target - containers[index], index + 1) +
        countCombinations(containers, target, index + 1);
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    const containers = data.trim().split('\n').map(Number);

    console.log(countCombinations(containers, 150, 0));
});