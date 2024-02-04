const fs = require('fs');
const crypto = require('crypto');

const secretKey = fs.readFileSync('input.txt', 'utf8').trim();
let number = 0;

while (true) {
  const hash = crypto.createHash('md5').update(secretKey + number).digest('hex');
  
  if (hash.startsWith('000000')) {
    console.log(number);
    break;
  }
  
  number++;
}