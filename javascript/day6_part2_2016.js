const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n').map((line) => line.trim());

const messages = input.filter((line) => line.length > 0);

const originalMessage = getOriginalMessage(messages);
console.log(originalMessage);

function getOriginalMessage(messages) {
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

    let originalMessage = '';
    for (const charCount of count) {
        originalMessage += getLeastCommonChar(charCount);
    }

    return originalMessage;
}

function getLeastCommonChar(count) {
    let minChar;
    let minCount = Number.MAX_SAFE_INTEGER;

    for (const char in count) {
        if (count[char] < minCount) {
            minCount = count[char];
            minChar = char;
        }
    }

    return minChar;
}