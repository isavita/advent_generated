const fs = require('fs');

function readInput(filename: string): [string, string[][]] {
  const data = fs.readFileSync(filename, 'utf8');
  const lines: string[] = data.split('\n');
  const algorithm: string = lines[0].replace('\n', '');
  const image: string[][] = lines.slice(2).map((line: string) => line.split(''));
  return [algorithm, image];
}

function enhanceImage(image: string[][], algorithm: string, times: number): string[][] {
  for (let i = 0; i < times; i++) {
    image = applyAlgorithm(image, algorithm, i % 2 === 1 && algorithm[0] === '#');
  }
  return image;
}

function applyAlgorithm(image: string[][], algorithm: string, flip: boolean): string[][] {
  const enhancedImage: string[][] = Array(image.length + 2).fill(0).map(() => Array(image[0].length + 2).fill(''));
  for (let i = 0; i < enhancedImage.length; i++) {
    for (let j = 0; j < enhancedImage[i].length; j++) {
      const index: number = calculateIndex(i - 1, j - 1, image, flip);
      enhancedImage[i][j] = algorithm[index];
    }
  }
  return enhancedImage;
}

function calculateIndex(i: number, j: number, image: string[][], flip: boolean): number {
  let index: number = 0;
  for (let di = -1; di <= 1; di++) {
    for (let dj = -1; dj <= 1; dj++) {
      index <<= 1;
      if (i + di >= 0 && i + di < image.length && j + dj >= 0 && j + dj < image[0].length) {
        if (image[i + di][j + dj] === '#') {
          index |= 1;
        }
      } else if (flip) {
        index |= 1;
      }
    }
  }
  return index;
}

function countLitPixels(image: string[][]): number {
  let count: number = 0;
  for (const row of image) {
    for (const pixel of row) {
      if (pixel === '#') {
        count++;
      }
    }
  }
  return count;
}

const [algorithm, image] = readInput('input.txt');
const enhancedImage = enhanceImage(image, algorithm, 2);
console.log(countLitPixels(enhancedImage));