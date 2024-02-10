const fs = require('fs');
const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

class Num {
    constructor(pos, val) {
        this.pos = pos;
        this.val = val;
    }
}

function mix(nums) {
    const n = nums.length - 1;
    for (let i = 0; i < nums.length; i++) {
        const oldpos = nums[i].pos;
        const newpos = ((oldpos + nums[i].val) % n + n) % n;
        if (oldpos < newpos) {
            for (let j = 0; j < nums.length; j++) {
                if (nums[j].pos > oldpos && nums[j].pos <= newpos) {
                    nums[j].pos--;
                }
            }
        }
        if (newpos < oldpos) {
            for (let j = 0; j < nums.length; j++) {
                if (nums[j].pos >= newpos && nums[j].pos < oldpos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newpos;
    }
}

function coords(nums) {
    const l = nums.length;
    let zeroPos = 0;
    for (let i = 0; i < nums.length; i++) {
        if (nums[i].val === 0) {
            zeroPos = nums[i].pos;
            break;
        }
    }
    let sum = 0;
    for (let i = 0; i < nums.length; i++) {
        if (nums[i].pos === (zeroPos + 1000) % l || nums[i].pos === (zeroPos + 2000) % l || nums[i].pos === (zeroPos + 3000) % l) {
            sum += nums[i].val;
        }
    }
    return sum;
}

const nums = data.map((n, i) => new Num(i, parseInt(n)));
const nums2 = nums.map(n => new Num(n.pos, 811589153 * n.val));

mix(nums);
console.log(coords(nums));