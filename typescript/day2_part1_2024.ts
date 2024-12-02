
import fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.trim().split('\n');
let safeReportCount = 0;

for (const line of lines) {
  const levels = line.split(/\s+/).map(Number);
  if (isSafeReport(levels)) {
    safeReportCount++;
  }
}

console.log(safeReportCount);


function isSafeReport(levels: number[]): boolean {
  if (levels.length < 2) return false;
  const firstDiff = levels[1] - levels[0];
  if (firstDiff === 0) return false;
  const isIncreasing = firstDiff > 0;

  for (let i = 0; i < levels.length - 1; i++) {
    const diff = levels[i + 1] - levels[i];
    if (diff === 0 || (isIncreasing && diff <= 0) || (!isIncreasing && diff >= 0) || Math.abs(diff) < 1 || Math.abs(diff) > 3) return false;
  }
  return true;
}
