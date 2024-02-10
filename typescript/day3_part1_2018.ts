const fs = require('fs');

class Claim {
  constructor(id, left, top, width, height) {
    this.id = id;
    this.left = left;
    this.top = top;
    this.width = width;
    this.height = height;
  }
}

function parseClaim(s) {
  const [id, left, top, width, height] = s.match(/\d+/g).map(Number);
  return new Claim(id, left, top, width, height);
}

function readClaims(filename) {
  const input = fs.readFileSync(filename, 'utf8').trim().split('\n');
  return input.map(parseClaim);
}

function countOverlappingInches(claims) {
  const fabric = new Map();
  claims.forEach(claim => {
    for (let i = claim.left; i < claim.left + claim.width; i++) {
      for (let j = claim.top; j < claim.top + claim.height; j++) {
        const coord = `${i},${j}`;
        fabric.set(coord, (fabric.get(coord) || 0) + 1);
      }
    }
  });

  let overlapping = 0;
  for (const count of fabric.values()) {
    if (count > 1) {
      overlapping++;
    }
  }
  return overlapping;
}

const claims = readClaims('input.txt');
const overlapping = countOverlappingInches(claims);
console.log(overlapping);