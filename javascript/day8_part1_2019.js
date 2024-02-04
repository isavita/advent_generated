const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim();

const width = 25;
const height = 6;
const layerSize = width * height;

let minZeros = layerSize + 1;
let result = 0;

for (let i = 0; i < data.length; i += layerSize) {
  const layer = data.slice(i, i + layerSize);
  let zeroCount = 0;
  let oneCount = 0;
  let twoCount = 0;

  for (let j = 0; j < layer.length; j++) {
    switch (layer[j]) {
      case '0':
        zeroCount++;
        break;
      case '1':
        oneCount++;
        break;
      case '2':
        twoCount++;
        break;
    }
  }

  if (zeroCount < minZeros) {
    minZeros = zeroCount;
    result = oneCount * twoCount;
  }
}

console.log(result);