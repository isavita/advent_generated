const fs = require('fs');

function transform(subjectNumber, loopSize) {
    let value = 1;
    for (let i = 0; i < loopSize; i++) {
        value *= subjectNumber;
        value %= 20201227;
    }
    return value;
}

function findLoopSize(publicKey) {
    let value = 1;
    let loopSize = 0;
    while (value !== publicKey) {
        value *= 7;
        value %= 20201227;
        loopSize++;
    }
    return loopSize;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const input = data.split('\n');
    const cardPublicKey = parseInt(input[0]);
    const doorPublicKey = parseInt(input[1]);

    const cardLoopSize = findLoopSize(cardPublicKey);
    const encryptionKey = transform(doorPublicKey, cardLoopSize);

    console.log(encryptionKey);
});