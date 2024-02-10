const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const messages = input.map(message => message.trim());

const originalMessage = getOriginalMessage(messages);
console.log(originalMessage);

function getOriginalMessage(messages) {
    if (messages.length === 0) {
        return '';
    }
    const messageLength = messages[0].length;
    const count = Array.from({ length: messageLength }, () => ({}));

    messages.forEach(message => {
        message.split('').forEach((char, index) => {
            count[index][char] = (count[index][char] || 0) + 1;
        });
    });

    let originalMessage = '';
    count.forEach(charCount => {
        originalMessage += getLeastCommonChar(charCount);
    });

    return originalMessage;
}

function getLeastCommonChar(count) {
    let minChar;
    let minCount = Number.MAX_SAFE_INTEGER;

    Object.entries(count).forEach(([char, cnt]) => {
        if (cnt < minCount) {
            minCount = cnt;
            minChar = char;
        }
    });

    return minChar;
}