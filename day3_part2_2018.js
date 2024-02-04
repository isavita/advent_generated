const fs = require('fs');

class Claim {
    constructor(id, x, y, width, height) {
        this.id = id;
        this.x = x;
        this.y = y;
        this.width = width;
        this.height = height;
    }
}

function readClaims(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const claims = [];

    for (let i = 0; i < data.length; i++) {
        const parts = data[i].split(' ');
        const id = parseInt(parts[0].substring(1));
        const coords = parts[2].substring(0, parts[2].length - 1).split(',');
        const x = parseInt(coords[0]);
        const y = parseInt(coords[1]);
        const dims = parts[3].split('x');
        const width = parseInt(dims[0]);
        const height = parseInt(dims[1]);

        claims.push(new Claim(id, x, y, width, height));
    }

    return claims;
}

const claims = readClaims('input.txt');
const fabric = Array.from({ length: 1000 }, () => Array(1000).fill(0));

for (const claim of claims) {
    for (let y = claim.y; y < claim.y + claim.height; y++) {
        for (let x = claim.x; x < claim.x + claim.width; x++) {
            fabric[y][x]++;
        }
    }
}

for (const claim of claims) {
    let overlap = false;
    for (let y = claim.y; y < claim.y + claim.height; y++) {
        for (let x = claim.x; x < claim.x + claim.width; x++) {
            if (fabric[y][x] > 1) {
                overlap = true;
                break;
            }
        }
        if (overlap) {
            break;
        }
    }
    if (!overlap) {
        console.log(claim.id);
        break;
    }
}