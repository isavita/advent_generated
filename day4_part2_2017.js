const fs = require('fs');

function sortString(w) {
    return w.split('').sort().join('');
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error('File reading error', err);
        return;
    }

    const passphrases = data.trim().split('\n');
    let validCount = 0;

    passphrases.forEach(passphrase => {
        const words = passphrase.split(' ');
        const wordSet = {};

        let valid = true;
        words.forEach(word => {
            const sortedWord = sortString(word);
            if (wordSet[sortedWord]) {
                valid = false;
                return;
            }
            wordSet[sortedWord] = true;
        });

        if (valid) {
            validCount++;
        }
    });

    console.log(validCount);
});