
const fs = require('fs');
const crypto = require('crypto');

const data = fs.readFileSync('input.txt', 'utf8').trim();
let number = 0;

while (true) {
    const hash = crypto.createHash('md5').update(data + number.toString()).digest('hex');

    if (hash.startsWith('00000')) {
        console.log(number);
        break;
    }

    number++;
}
