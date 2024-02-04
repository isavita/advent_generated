const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

let twoCount = 0;
let threeCount = 0;

data.forEach(id => {
    const [twos, threes] = countTwosAndThrees(id);
    if (twos) {
        twoCount++;
    }
    if (threes) {
        threeCount++;
    }
});

const checksum = twoCount * threeCount;
console.log(checksum);

function countTwosAndThrees(id) {
    const charCount = {};
    for (let char of id) {
        charCount[char] = (charCount[char] || 0) + 1;
    }

    let hasTwos = false;
    let hasThrees = false;
    for (let count of Object.values(charCount)) {
        if (count === 2) {
            hasTwos = true;
        } else if (count === 3) {
            hasThrees = true;
        }
    }
    return [hasTwos, hasThrees];
}