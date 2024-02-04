
const fs = require('fs');

function hexToBin(hex) {
  let bin = "";
  hex.split('').forEach((h) => {
    const b = parseInt(h, 16).toString(2).padStart(4, '0');
    bin += b;
  });
  return bin;
}

function parsePacket(binStr, idx) {
  let version = parseInt(binStr[idx]) << 2 | parseInt(binStr[idx + 1]) << 1 | parseInt(binStr[idx + 2]);
  let typeID = parseInt(binStr[idx + 3]) << 2 | parseInt(binStr[idx + 4]) << 1 | parseInt(binStr[idx + 5]);
  idx += 6;

  if (typeID === 4) {
    let value = BigInt(0);
    while (binStr[idx] === '1') {
      value = (value << BigInt(4)) | (BigInt(parseInt(binStr[idx + 1])) << BigInt(3)) | (BigInt(parseInt(binStr[idx + 2])) << BigInt(2)) | (BigInt(parseInt(binStr[idx + 3])) << BigInt(1)) | BigInt(parseInt(binStr[idx + 4]));
      idx += 5;
    }
    value = (value << BigInt(4)) | (BigInt(parseInt(binStr[idx + 1])) << BigInt(3)) | (BigInt(parseInt(binStr[idx + 2])) << BigInt(2)) | (BigInt(parseInt(binStr[idx + 3])) << BigInt(1)) | BigInt(parseInt(binStr[idx + 4]));
    idx += 5;
    return [version, idx, value];
  }

  let lengthTypeID = parseInt(binStr[idx]);
  idx++;
  let numSubPackets, subPacketLength;

  if (lengthTypeID === 0) {
    subPacketLength = 0;
    for (let i = 0; i < 15; i++) {
      subPacketLength = (subPacketLength << 1) | parseInt(binStr[idx]);
      idx++;
    }
  } else {
    numSubPackets = 0;
    for (let i = 0; i < 11; i++) {
      numSubPackets = (numSubPackets << 1) | parseInt(binStr[idx]);
      idx++;
    }
  }

  let values = [];
  while (true) {
    if (lengthTypeID === 0 && subPacketLength === 0) {
      break;
    }
    if (lengthTypeID === 1 && numSubPackets === 0) {
      break;
    }
    let [_, newIndex, subValue] = parsePacket(binStr, idx);
    values.push(subValue);

    if (lengthTypeID === 0) {
      subPacketLength -= newIndex - idx;
    } else {
      numSubPackets--;
    }
    idx = newIndex;
  }

  let result;
  switch (typeID) {
    case 0:
      result = BigInt(0);
      values.forEach((value) => {
        result += value;
      });
      break;
    case 1:
      result = BigInt(1);
      values.forEach((value) => {
        result *= value;
      });
      break;
    case 2:
      result = values[0];
      values.forEach((value) => {
        if (value < result) {
          result = value;
        }
      });
      break;
    case 3:
      result = values[0];
      values.forEach((value) => {
        if (value > result) {
          result = value;
        }
      });
      break;
    case 5:
      result = BigInt(0);
      if (values[0] > values[1]) {
        result = BigInt(1);
      }
      break;
    case 6:
      result = BigInt(0);
      if (values[0] < values[1]) {
        result = BigInt(1);
      }
      break;
    case 7:
      result = BigInt(0);
      if (values[0] === values[1]) {
        result = BigInt(1);
      }
      break;
    default:
      throw new Error("Unknown typeID");
  }

  return [version, idx, result];
}

const input = fs.readFileSync('input.txt', 'utf8').trim();
const binStr = hexToBin(input);
const [_, __, value] = parsePacket(binStr, 0);
console.log(value.toString());
