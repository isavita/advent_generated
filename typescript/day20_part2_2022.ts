
import * as fs from 'fs';

interface Num {
    pos: number;
    val: number;
}

function main() {
    const input = fs.readFileSync("input.txt", "utf-8").trim().split("\n").map(Number);
    let nums: Num[] = input.map((val, pos) => ({ pos, val }));
    let nums2: Num[] = nums.map(num => ({ pos: num.pos, val: 811589153 * num.val }));

    for (let i = 0; i < 10; i++) {
        mix(nums2);
    }
    console.log(coords(nums2));
}

function mix(nums: Num[]) {
    const n = nums.length;
    for (let i = 0; i < n; i++) {
        const oldPos = nums[i].pos;
        let newPos = ((oldPos + nums[i].val) % (n - 1) + (n - 1)) % (n - 1);
        if (oldPos < newPos) {
            for (let j = 0; j < n; j++) {
                if (nums[j].pos > oldPos && nums[j].pos <= newPos) {
                    nums[j].pos--;
                }
            }
        } else if (newPos < oldPos) {
            for (let j = 0; j < n; j++) {
                if (nums[j].pos >= newPos && nums[j].pos < oldPos) {
                    nums[j].pos++;
                }
            }
        }
        nums[i].pos = newPos;
    }
}

function coords(nums: Num[]): number {
    const l = nums.length;
    let zeroPos = nums.find(num => num.val === 0)!.pos;
    let sum = 0;
    for (let i = 0; i < l; i++) {
        const pos = nums[i].pos;
        if (pos === (zeroPos + 1000) % l || pos === (zeroPos + 2000) % l || pos === (zeroPos + 3000) % l) {
            sum += nums[i].val;
        }
    }
    return sum;
}

main();
