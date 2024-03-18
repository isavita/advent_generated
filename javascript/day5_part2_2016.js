const fs = require('fs');
const crypto = require('crypto');

function md5Hash(input) {
  return crypto.createHash('md5').update(input).digest('hex');
}

function findPassword(doorID) {
  let password = Array(8).fill('');
  let filledPositions = 0;
  let found = Array(8).fill(false);

  for (let i = 0; filledPositions < 8; i++) {
    let hash = md5Hash(doorID + i);
    if (hash.startsWith('00000')) {
      let pos = parseInt(hash[5], 10);
      if (pos >= 0 && pos <= 7) {
        if (!found[pos]) {
          found[pos] = true;
          password[pos] = hash[6];
          filledPositions++;
        }
      }
    }
  }

  return password.join('');
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  let doorID = data.trim();
  let password = findPassword(doorID);
  console.log(password);
});