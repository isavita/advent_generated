
import * as fs from 'fs';

const data = fs.readFileSync('input.txt', 'utf-8').trim();
const width = 25;
const height = 6;
const layerSize = width * height;
const finalImage = Array(layerSize).fill('2');

for (let i = 0; i < data.length; i += layerSize) {
  const layer = data.substring(i, Math.min(i + layerSize, data.length));
  for (let j = 0; j < layer.length; j++) {
    if (finalImage[j] === '2') {
      finalImage[j] = layer[j];
    }
  }
}

let output = '';
for (let i = 0; i < height; i++) {
  for (let j = 0; j < width; j++) {
    output += finalImage[i * width + j] === '0' ? ' ' : '#';
  }
  output += '\n';
}
console.log(output);
