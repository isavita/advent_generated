const fs = require('fs');

function readFileToMatrix(filePath: string): string[][] {
  const fileContent = fs.readFileSync(filePath, 'utf8');
  return fileContent.split('\n').map((line: string) => line.split(''));
}

function sumOfPartNumbers(matrix: string[][]): number {
  let sum = 0; // changed from const to let
  const visited: boolean[][] = matrix.map(row => row.map(() => false));

  for (let y = 0; y < matrix.length; y++) {
    for (let x = 0; x < matrix[y].length; x++) {
      if (!visited[y][x] && !isNaN(Number(matrix[y][x]))) {
        const number = extractNumber(matrix, x, y);
        if (isAdjacentToSymbol(matrix, x, y, number.length)) {
          sum += Number(number);
        }
        for (let i = 0; i < number.length; i++) {
          visited[y][x + i] = true;
        }
      }
    }
  }
  return sum;
}

function extractNumber(matrix: string[][], x: number, y: number): string {
  let numberStr = '';
  while (x < matrix[y].length && !isNaN(Number(matrix[y][x]))) {
    numberStr += matrix[y][x];
    x++;
  }
  return numberStr;
}

function isAdjacentToSymbol(matrix: string[][], x: number, y: number, length: number): boolean {
  for (let i = 0; i < length; i++) {
    if (checkAdjacent(matrix, x + i, y)) {
      return true;
    }
  }
  return false;
}

function checkAdjacent(matrix: string[][], x: number, y: number): boolean {
  for (let dy = -1; dy <= 1; dy++) {
    for (let dx = -1; dx <= 1; dx++) {
      const adjX = x + dx;
      const adjY = y + dy;
      if (adjY >= 0 && adjY < matrix.length && adjX >= 0 && adjX < matrix[adjY].length) {
        if (isNaN(Number(matrix[adjY][adjX])) && matrix[adjY][adjX] !== '.') {
          return true;
        }
      }
    }
  }
  return false;
}

const matrix = readFileToMatrix('input.txt');
console.log(sumOfPartNumbers(matrix));