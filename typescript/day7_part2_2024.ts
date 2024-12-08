
import * as fs from 'fs';

const concat = (a: number, b: number): number => {
  return parseInt(`${a}${b}`);
};

const canProduce = (target: number, nums: number[], idx: number, value: number): boolean => {
  if (idx === nums.length) return value === target;
  const n = nums[idx];
  return (
    canProduce(target, nums, idx + 1, value + n) ||
    canProduce(target, nums, idx + 1, value * n) ||
    canProduce(target, nums, idx + 1, concat(value, n))
  );
};

const main = () => {
  const input = fs.readFileSync('input.txt', 'utf-8');
  let total = 0;
  input.split('\n').forEach((line) => {
    if (line.trim() === '') return;
    const [targetStr, numsStr] = line.split(':');
    const target = parseInt(targetStr.trim());
    const nums = numsStr
      .trim()
      .split(/\s+/)
      .map((n) => parseInt(n));

    if (nums.length === 1) {
      if (nums[0] === target) total += target;
      return;
    }
    if (canProduce(target, nums, 1, nums[0])) total += target;
  });
  console.log(total);
};

main();
