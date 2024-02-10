
const fs = require('fs');
const crypto = require('crypto');

const data = fs.readFileSync('input.txt', 'utf8');
const salt = data.trim();
let keys = 0;
let index = 0;

while (keys < 64) {
    const hash = getMD5Hash(salt + index);
    const triplet = findTriplet(hash);

    if (triplet !== '') {
        for (let i = 1; i <= 1000; i++) {
            const nextHash = getMD5Hash(salt + (index + i).toString());

            if (nextHash.includes(triplet.repeat(5))) {
                keys++;
                break;
            }
        }
    }
    index++;
}

console.log(index - 1);

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
