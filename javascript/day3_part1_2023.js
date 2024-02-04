const fs = require('fs');

const readFileToMatrix = (filePath) => {
  const data = fs.readFileSync(filePath, 'utf8');
  const lines = data.split('\n');
  const matrix = lines.map(line => line.split(''));
  return matrix;
};

const sumOfPartNumbers = (matrix) => {
  let sum = 0;
  const visited = new Array(matrix.length).fill(null).map(() => new Array(matrix[0].length).fill(false));

  matrix.forEach((row, y) => {
    row.forEach((cell, x) => {
      if (!visited[y][x] && !isNaN(cell)) {
        const { number, length } = extractNumber(matrix, x, y);
        if (isAdjacentToSymbol(matrix, x, y, length)) {
          sum += number;
        }
        for (let i = 0; i < length; i++) {
          visited[y][x + i] = true;
        }
      }
    });
  });

  return sum;
};

const extractNumber = (matrix, x, y) => {
  let numberStr = '';
  while (x < matrix[y].length && !isNaN(matrix[y][x])) {
    numberStr += matrix[y][x];
    x++;
  }
  const number = parseInt(numberStr);
  return { number, length: numberStr.length };
};

const isAdjacentToSymbol = (matrix, x, y, length) => {
  for (let i = 0; i < length; i++) {
    if (checkAdjacent(matrix, x + i, y)) {
      return true;
    }
  }
  return false;
};

const checkAdjacent = (matrix, x, y) => {
  for (let dy = -1; dy <= 1; dy++) {
    for (let dx = -1; dx <= 1; dx++) {
      const adjX = x + dx;
      const adjY = y + dy;
      if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length) {
        if (isNaN(matrix[adjY][adjX]) && matrix[adjY][adjX] !== '.') {
          return true;
        }
      }
    }
  }
  return false;
};

const matrix = readFileToMatrix('input.txt');
const sum = sumOfPartNumbers(matrix);
console.log(sum);