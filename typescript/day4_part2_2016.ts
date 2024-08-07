const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').split('\n');

for (const line of input) {
  if (isRealRoom(line)) {
    const decryptedName = decryptName(line);
    if (decryptedName.includes('northpole object')) {
      console.log(getSectorID(line));
      break;
    }
  }
}

function isRealRoom(room: string): boolean {
  const [encryptedName, checksum] = room.split('[');
  const nameParts = encryptedName.split('-').slice(0, -1);
  const letterCounts: { [key: string]: number } = {};

  for (const part of nameParts) {
    for (const letter of part) {
      letterCounts[letter] = (letterCounts[letter] || 0) + 1;
    }
  }

  const sortedCounts = Object.entries(letterCounts).sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]));
  return sortedCounts.slice(0, 5).every(([letter], i) => letter === checksum[i]);
}

function getSectorID(room: string): number {
  const parts = room.split('-');
  const sectorIDPart = parts[parts.length - 1];
  return parseInt(sectorIDPart.split('[')[0]);
}

function decryptName(room: string): string {
  const parts = room.split('-');
  const sectorIDPart = parts[parts.length - 1];
  const sectorID = parseInt(sectorIDPart.split('[')[0]);
  let decryptedName = '';

  for (const part of parts.slice(0, -1)) {
    for (const letter of part) {
      if (letter === '-') {
        decryptedName += ' ';
      } else {
        const shiftedLetter = String.fromCharCode((letter.charCodeAt(0) - 97 + sectorID) % 26 + 97);
        decryptedName += shiftedLetter;
      }
    }
    decryptedName += ' ';
  }

  return decryptedName.trim();
}