
const fs = require('fs');

function trimLeadingZeros(s) {
  let i = 0;
  while (i < s.length - 1 && s[i] === '0') {
    i++;
  }
  return s.substring(i);
}

function splitStone(s) {
  const mid = Math.floor(s.length / 2);
  let left = trimLeadingZeros(s.substring(0, mid));
  let right = trimLeadingZeros(s.substring(mid));
  if (left === "") left = "0";
  if (right === "") right = "0";
  return [left, right];
}

function multiplyBy2024(s) {
    const num = s.split('').map(Number);
    const multiplier = [2, 0, 2, 4];
    const result = new Array(num.length + multiplier.length).fill(0);

    for (let i = num.length - 1; i >= 0; i--) {
        let carry = 0;
        for (let j = multiplier.length - 1; j >= 0; j--) {
            const product = (num[i] * multiplier[j]) + result[i + j + 1] + carry;
            result[i + j + 1] = product % 10;
            carry = Math.floor(product / 10);
        }
        result[i] += carry;
    }

    let start = 0;
    while (start < result.length - 1 && result[start] === 0) {
        start++;
    }
    return result.slice(start).join('');
}


function main() {
  try {
    const data = fs.readFileSync('input.txt', 'utf8');
    const stonesStr = data.trim().split(/\s+/);

    let stonesMap = new Map();
    for (const s of stonesStr) {
      stonesMap.set(s, (stonesMap.get(s) || 0) + 1);
    }

    const steps = 75;
    for (let step = 0; step < steps; step++) {
      const newStonesMap = new Map();
      for (const [stone, count] of stonesMap) {
        if (stone === "0") {
          newStonesMap.set("1", (newStonesMap.get("1") || 0) + count);
        } else if (stone.length % 2 === 0) {
          const [left, right] = splitStone(stone);
          newStonesMap.set(left, (newStonesMap.get(left) || 0) + count);
          newStonesMap.set(right, (newStonesMap.get(right) || 0) + count);
        } else {
          const newStone = multiplyBy2024(stone);
          newStonesMap.set(newStone, (newStonesMap.get(newStone) || 0) + count);
        }
      }
      stonesMap = newStonesMap;
    }

    let totalStones = 0;
    for (const count of stonesMap.values()) {
      totalStones += count;
    }

    console.log(totalStones);
  } catch (err) {
    console.error("Error:", err);
  }
}

main();
