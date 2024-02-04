const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

let sumOfSectorIDs = 0;

for (let i = 0; i < data.length; i++) {
    const line = data[i];
    if (isRealRoom(line)) {
        sumOfSectorIDs += getSectorID(line);
    }
}

console.log(sumOfSectorIDs);

function isRealRoom(room) {
    const parts = room.split("[");
    const checksum = parts[1].replace("]", "");
    let encryptedName = parts[0].split("-");
    encryptedName = encryptedName.slice(0, encryptedName.length - 1);

    const letterCounts = {};
    for (let part of encryptedName) {
        for (let letter of part) {
            if (letterCounts[letter]) {
                letterCounts[letter]++;
            } else {
                letterCounts[letter] = 1;
            }
        }
    }

    const counts = [];
    for (let letter in letterCounts) {
        counts.push({ letter: letter, count: letterCounts[letter] });
    }

    counts.sort((a, b) => {
        if (a.count === b.count) {
            return a.letter.localeCompare(b.letter);
        }
        return b.count - a.count;
    });

    for (let i = 0; i < checksum.length; i++) {
        if (checksum[i] !== counts[i].letter) {
            return false;
        }
    }

    return true;
}

function getSectorID(room) {
    const parts = room.split("-");
    const sectorIDPart = parts[parts.length - 1];
    const sectorID = parseInt(sectorIDPart.split("[")[0]);
    return sectorID;
}