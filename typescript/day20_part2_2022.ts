const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');
const nums = input.map((n, i) => ({ pos: i, val: parseInt(n) }));
const nums2 = nums.map(n => ({ pos: n.pos, val: 811589153 * n.val }));

for (let i = 0; i < 10; i++) {
    mix(nums2);
}

console.log(coords(nums2));

function mix(nums) {
    const n = nums.length - 1;
    nums.forEach((num, i) => {
        const oldpos = num.pos;
        const newpos = ((oldpos + num.val) % n + n) % n;
        if (oldpos < newpos) {
            nums.forEach((num2, j) => {
                if (num2.pos > oldpos && num2.pos <= newpos) {
                    num2.pos--;
                }
            });
        }
        if (newpos < oldpos) {
            nums.forEach((num2, j) => {
                if (num2.pos >= newpos && num2.pos < oldpos) {
                    num2.pos++;
                }
            });
        }
        num.pos = newpos;
    });
}

function coords(nums) {
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
}