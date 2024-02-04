const fs = require('fs');

const diskLength = 35651584;

const initialState = fs.readFileSync('input.txt', 'utf8').trim();

const data = generateData(initialState, diskLength);

const checksum = calculateChecksum(data);

console.log("Checksum:", checksum);

function generateData(initialState, length) {
  let data = initialState;
  while (data.length < length) {
    let b = '';
    for (let i = data.length - 1; i >= 0; i--) {
      if (data[i] === '0') {
        b += '1';
      } else {
        b += '0';
      }
    }
    data = data + '0' + b;
  }
  return data.slice(0, length);
}

function calculateChecksum(data) {
  while (data.length % 2 === 0) {
    let b = '';
    for (let i = 0; i < data.length; i += 2) {
      if (data[i] === data[i + 1]) {
        b += '1';
      } else {
        b += '0';
      }
    }
    data = b;
  }
  return data;
}