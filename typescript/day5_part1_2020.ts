
const fs = require('fs');

function main() {
  const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

  let maxSeatID = 0;
  for (const pass of input) {
    let decodedPass = pass.replace(/F/g, '0').replace(/B/g, '1').replace(/L/g, '0').replace(/R/g, '1');
    let seatID = decode(decodedPass);
    if (seatID > maxSeatID) {
      maxSeatID = seatID;
    }
  }

  console.log(maxSeatID);
}

function decode(pass) {
  let row = binaryToInt(pass.slice(0, 7));
  let column = binaryToInt(pass.slice(7));
  return row * 8 + column;
}

function binaryToInt(binaryStr) {
  let result = 0;
  for (let i = 0; i < binaryStr.length; i++) {
    if (binaryStr[i] === '1') {
      result |= 1 << (binaryStr.length - i - 1);
    }
  }
  return result;
}

main();
