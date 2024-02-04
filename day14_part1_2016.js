const fs = require('fs');
const crypto = require('crypto');

const data = fs.readFileSync('input.txt', 'utf8').trim();
let keys = 0;
let index = 0;

while (keys < 64) {
    const salt = data + index;
    const hash = crypto.createHash('md5').update(salt).digest('hex');

    const triplet = findTriplet(hash);
    if (triplet !== "") {
        for (let i = 1; i <= 1000; i++) {
            const nextSalt = data + (index + i);
            const nextHash = crypto.createHash('md5').update(nextSalt).digest('hex');

            if (nextHash.includes(triplet.repeat(5))) {
                keys++;
                break;
            }
        }
    }
    index++;
}

console.log(index - 1);

function findTriplet(hash) {
    for (let i = 0; i < hash.length - 2; i++) {
        if (hash[i] === hash[i + 1] && hash[i] === hash[i + 2]) {
            return hash[i];
        }
    }
    return "";
}