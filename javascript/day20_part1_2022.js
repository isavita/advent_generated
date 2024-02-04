const fs = require('fs');

const nums = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map((n, i) => ({ pos: i, val: parseInt(n) }));
const nums2 = nums.map(({ pos, val }) => ({ pos, val: 811589153 * val }));

mix(nums);
console.log(coords(nums));

function mix(nums) {
  const n = nums.length - 1;
  nums.forEach((num) => {
    const oldpos = num.pos;
    const newpos = ((oldpos + num.val) % n + n) % n;
    if (oldpos < newpos) {
      nums.forEach((n) => {
        if (n.pos > oldpos && n.pos <= newpos) {
          n.pos--;
        }
      });
    }
    if (newpos < oldpos) {
      nums.forEach((n) => {
        if (n.pos >= newpos && n.pos < oldpos) {
          n.pos++;
        }
      });
    }
    num.pos = newpos;
  });
}

function coords(nums) {
  const l = nums.length;
  let zeroPos;
  for (const num of nums) {
    if (num.val === 0) {
      zeroPos = num.pos;
      break;
    }
  }
  let sum = 0;
  for (const num of nums) {
    if (num.pos === (zeroPos + 1000) % l || num.pos === (zeroPos + 2000) % l || num.pos === (zeroPos + 3000) % l) {
      sum += num.val;
    }
  }
  return sum;
}