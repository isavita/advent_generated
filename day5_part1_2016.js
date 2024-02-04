const fs = require('fs');
const crypto = require('crypto');

const input = fs.readFileSync('input.txt', 'utf8').trim();

function findPassword(doorID) {
  let password = '';
  let i = 0;
  while (password.length < 8) {
    const hash = crypto.createHash('md5').update(doorID + i).digest('hex');
    if (hash.startsWith('00000')) {
      password += hash[5];
    }
    i++;
  }
  return password;
}

const doorID = input.trim();
const password = findPassword(doorID);
console.log(password);