const fs = require('fs');

const readAll = (path) => {
  return fs.readFileSync(path, 'utf8').trim();
};

const toInt = (s) => {
  return parseInt(s);
};

const mix = (nums) => {
  const n = nums.length - 1;
  nums.forEach((num, i) => {
    const oldpos = num.pos;
    const newpos = ((oldpos + num.val) % n + n) % n;
    if (oldpos < newpos) {
      nums.forEach((num2) => {
        if (num2.pos > oldpos && num2.pos <= newpos) {
          num2.pos--;
        }
      });
    }
    if (newpos < oldpos) {
      nums.forEach((num2) => {
        if (num2.pos >= newpos && num2.pos < oldpos) {
          num2.pos++;
        }
      });
    }
    num.pos = newpos;
  });
};

const coords = (nums) => {
  const l = nums.length;
  let zeroPos;
  for (let i = 0; i < l; i++) {
    if (nums[i].val === 0) {
      zeroPos = nums[i].pos;
      break;
    }
  }
  let sum = 0;
  for (let i = 0; i < l; i++) {
    if (nums[i].pos === (zeroPos + 1000) % l || nums[i].pos === (zeroPos + 2000) % l || nums[i].pos === (zeroPos + 3000) % l) {
      sum += nums[i].val;
    }
  }
  return sum;
};

const input = readAll('input.txt').split('\n');
const nums = input.map((n, i) => ({ pos: i, val: toInt(n) }));
const nums2 = nums.map((num) => ({ pos: num.pos, val: 811589153 * num.val }));

for (let i = 0; i < 10; i++) {
  mix(nums2);
}

console.log(coords(nums2));