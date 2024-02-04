const fs = require('fs');

function parseClaim(s) {
    const c = {};
    const parts = s.split(' ');
    c.id = parseInt(parts[0].slice(1));
    const pos = parts[2].split(',');
    c.left = parseInt(pos[0]);
    c.top = parseInt(pos[1].slice(0, -1));
    const size = parts[3].split('x');
    c.width = parseInt(size[0]);
    c.height = parseInt(size[1]);
    return c;
}

function readClaims(filename) {
    const data = fs.readFileSync(filename, 'utf8');
    const lines = data.split('\n');
    const claims = [];
    for (let line of lines) {
        if (line.trim() !== '') {
            claims.push(parseClaim(line));
        }
    }
    return claims;
}

function countOverlappingInches(claims) {
    const fabric = {};
    for (let claim of claims) {
        for (let i = claim.left; i < claim.left + claim.width; i++) {
            for (let j = claim.top; j < claim.top + claim.height; j++) {
                const coord = i + ',' + j;
                fabric[coord] = (fabric[coord] || 0) + 1;
            }
        }
    }

    let overlapping = 0;
    for (let count of Object.values(fabric)) {
        if (count > 1) {
            overlapping++;
        }
    }
    return overlapping;
}

const claims = readClaims("input.txt");
const overlapping = countOverlappingInches(claims);
console.log(overlapping);