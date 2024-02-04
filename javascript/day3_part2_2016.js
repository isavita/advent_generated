const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8');
const rows = input.trim().split('\n');
const triangles = [];

for (let i = 0; i < rows.length; i += 3) {
    const sides = [[], [], []];
    for (let j = 0; j < 3; j++) {
        const parts = rows[i + j].trim().split(/\s+/);
        for (let k = 0; k < 3; k++) {
            sides[k].push(parseInt(parts[k]));
        }
    }
    triangles.push(...sides);
}

let count = 0;

for (const triangle of triangles) {
    triangle.sort((a, b) => a - b);
    if (triangle[0] + triangle[1] > triangle[2]) {
        count++;
    }
}

console.log(count);