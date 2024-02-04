const fs = require('fs');

function isRealRoom(room) {
  const parts = room.split("[");
  const checksum = parts[1].slice(0, -1);
  const encryptedName = parts[0].split("-");
  encryptedName.pop();

  const letterCounts = {};
  encryptedName.forEach(part => {
    part.split("").forEach(letter => {
      letterCounts[letter] = (letterCounts[letter] || 0) + 1;
    });
  });

  const counts = Object.entries(letterCounts).map(([letter, count]) => ({ letter, count }));

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

function decryptName(room) {
  const parts = room.split("-");
  const sectorIDPart = parts[parts.length - 1];
  const sectorID = parseInt(sectorIDPart.split("[")[0]);
  let decryptedName = "";

  parts.slice(0, -1).forEach(part => {
    part.split("").forEach(letter => {
      if (letter === '-') {
        decryptedName += ' ';
      } else {
        const shiftedLetter = String.fromCharCode(((letter.charCodeAt(0) - 97 + sectorID) % 26) + 97);
        decryptedName += shiftedLetter;
      }
    });
    decryptedName += ' ';
  });

  return decryptedName.trim();
}

fs.readFile('input.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  const lines = data.split('\n');
  for (const line of lines) {
    if (isRealRoom(line)) {
      const decryptedName = decryptName(line);
      if (decryptedName.includes("northpole object")) {
        console.log(getSectorID(line));
        break;
      }
    }
  }
});