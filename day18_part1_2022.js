const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let surfaceArea = 0;

for (let i = 0; i < input.length; i++) {
    const [x, y, z] = input[i].split(',').map(Number);

    surfaceArea += 6;

    if (input.includes(`${x+1},${y},${z}`)) surfaceArea--;
    if (input.includes(`${x-1},${y},${z}`)) surfaceArea--;
    if (input.includes(`${x},${y+1},${z}`)) surfaceArea--;
    if (input.includes(`${x},${y-1},${z}`)) surfaceArea--;
    if (input.includes(`${x},${y},${z+1}`)) surfaceArea--;
    if (input.includes(`${x},${y},${z-1}`)) surfaceArea--;
}

console.log(surfaceArea);