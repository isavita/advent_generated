const fs = require('fs');
const crypto = require('crypto');

const doorID = fs.readFileSync('input.txt', 'utf8').trim();

function findPassword(doorID) {
  let password = '';
  for (let i = 0; password.length < 8; i++) {
    const hash = md5Hash(doorID + i);
    if (hash.startsWith('00000')) {
      password += hash[5];
    }
  }
  return password;
}

function md5Hash(input) {
  const hash = crypto.createHash('md5');
  hash.update(input);
  return hash.digest('hex');
}

console.log(findPassword(doorID));