
const fs = require('node:fs');

function parseLevels(line) {
  return line.trim().split(/\s+/).map(Number);
}

function isSafeReport(levels) {
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

function isSafeWithOneRemoval(levels) {
  for (let i = 0; i < levels.length; i++) {
    const modifiedLevels = [...levels.slice(0, i), ...levels.slice(i + 1)];
    if (isSafeReport(modifiedLevels)) return true;
  }
  return false;
}

async function main() {
  try {
    const data = await fs.promises.readFile('input.txt', 'utf8');
    const lines = data.split('\n');
    let safeReportCount = 0;
    for (const line of lines) {
      if (line.trim() === "") continue;
      const levels = parseLevels(line);
      if (isSafeReport(levels) || isSafeWithOneRemoval(levels)) {
        safeReportCount++;
      }
    }
    console.log(safeReportCount);
  } catch (err) {
    console.error("Failed to read or process the file:", err);
  }
}

main();