const fs = require('fs');

const totalRows = 40;

function readFirstRow(filename) {
  return fs.readFileSync(filename, 'utf8').trim();
}

function countSafeTiles(firstRow, totalRows) {
  let currentRow = firstRow;
  let safeCount = countChar(currentRow, '.');

  for (let i = 1; i < totalRows; i++) {
    let nextRow = '';
    for (let j = 0; j < currentRow.length; j++) {
      if (isTrap(j - 1, j, j + 1, currentRow)) {
        nextRow += '^';
      } else {
        nextRow += '.';
        safeCount++;
      }
    }
    currentRow = nextRow;
  }
  return safeCount;
}

function isTrap(left, center, right, row) {
  let l = safeIfOutOfBounds(left, row);
  let c = row[center];
  let r = safeIfOutOfBounds(right, row);

  return (l === '^' && c === '^' && r === '.') ||
         (c === '^' && r === '^' && l === '.') ||
         (l === '^' && c === '.' && r === '.') ||
         (r === '^' && c === '.' && l === '.');
}

function safeIfOutOfBounds(index, row) {
  return (index < 0 || index >= row.length) ? '.' : row[index];
}

function countChar(str, char) {
  return str.split(char).length - 1;
}

function main() {
  let firstRow = readFirstRow('input.txt');
  let safeTilesCount = countSafeTiles(firstRow, totalRows);
  console.log(safeTilesCount);
}

main();