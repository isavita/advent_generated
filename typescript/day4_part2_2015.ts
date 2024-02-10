
const fs = require('fs');

const secretKey = fs.readFileSync('input.txt', 'utf8').trim();
let number = 0;

const crypto = require('crypto'); // Moved require statement outside of the loop

while (true) {
  const hash = crypto.createHash('md5').update(secretKey + number).digest('hex');

  if (hash.startsWith('000000')) {
    console.log(number);
    break;
  }

  number++;
}
