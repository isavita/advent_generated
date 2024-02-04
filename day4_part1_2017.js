const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const passphrases = data.trim().split('\n');
let validCount = 0;

passphrases.forEach(passphrase => {
    const words = passphrase.split(' ');
    const wordSet = new Set();

    let valid = true;
    words.forEach(word => {
        if (wordSet.has(word)) {
            valid = false;
            return;
        }
        wordSet.add(word);
    });

    if (valid) {
        validCount++;
    }
});

console.log(validCount);