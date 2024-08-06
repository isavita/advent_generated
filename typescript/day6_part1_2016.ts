const fs = require('fs');

function getCorrectedMessage(messages) {
    if (messages.length === 0) return '';
    const messageLength = messages[0].length;
    const count = Array(messageLength).fill(null).map(() => ({}));

    for (const message of messages) {
        for (let i = 0; i < message.length; i++) {
            const charCount = count[i];
            charCount[message[i]] = (charCount[message[i]] || 0) + 1;
        }
    }

    let correctedMessage = '';
    for (const charCount of count) {
        correctedMessage += getMostCommonChar(charCount);
    }

    return correctedMessage;
}

function getMostCommonChar(count) {
    let maxChar = '';
    let maxCount = 0;
    for (const char in count) {
        if (count[char] > maxCount) {
            maxCount = count[char];
            maxChar = char;
        }
    }
    return maxChar;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) throw err;
    const messages = data.split('\n');
    const correctedMessage = getCorrectedMessage(messages);
    console.log(correctedMessage);
});