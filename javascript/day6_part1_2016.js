const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').filter(Boolean);

const messages = input;
const correctedMessage = getCorrectedMessage(messages);
console.log(correctedMessage);

function getCorrectedMessage(messages) {
    if (messages.length === 0) {
        return '';
    }
    const messageLength = messages[0].length;
    const count = Array.from({ length: messageLength }, () => ({}));

    for (const message of messages) {
        for (let j = 0; j < message.length; j++) {
            const char = message[j];
            count[j][char] = (count[j][char] || 0) + 1;
        }
    }

    const correctedMessage = count.map(charCount => getMostCommonChar(charCount)).join('');

    return correctedMessage;
}

function getMostCommonChar(count) {
    let maxChar;
    let maxCount = 0;
    for (const char in count) {
        if (count[char] > maxCount) {
            maxCount = count[char];
            maxChar = char;
        }
    }
    return maxChar;
}