const fs = require('fs');
const crypto = require('crypto');

const hashCache = new Map();

function getStretchedMD5Hash(input) {
  if (hashCache.has(input)) {
    return hashCache.get(input);
  }
  let hash = getMD5Hash(input);
  for (let i = 0; i < 2016; i++) {
    hash = getMD5Hash(hash);
  }
  hashCache.set(input, hash);
  return hash;
}

function getMD5Hash(input) {
  return crypto.createHash('md5').update(input).digest('hex');
}

function findTriplet(hash) {
  for (let i = 0; i < hash.length - 2; i++) {
    if (hash[i] === hash[i + 1] && hash[i] === hash[i + 2]) {
      return hash[i];
    }
  }
  return '';
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const salt = data.trim();
  let keys = 0;
  let index = 0;

  while (keys < 64) {
    const hash = getStretchedMD5Hash(`${salt}${index}`);
    const triplet = findTriplet(hash);
    if (triplet !== '') {
      for (let i = 1; i <= 1000; i++) {
        const nextHash = getStretchedMD5Hash(`${salt}${index + i}`);
        if (nextHash.includes(triplet.repeat(5))) {
          keys++;
          break;
        }
      }
    }
    index++;
  }

  console.log(index - 1);
});