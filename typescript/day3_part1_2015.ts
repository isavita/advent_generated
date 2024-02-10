const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8');
const directions = data.trim();

const visitedHouses = new Map();
let x = 0, y = 0;

visitedHouses.set(`${x},${y}`, true);

for (let dir of directions) {
    switch (dir) {
        case '^':
            y++;
            break;
        case 'v':
            y--;
            break;
        case '>':
            x++;
            break;
        case '<':
            x--;
            break;
    }

    visitedHouses.set(`${x},${y}`, true);
}

console.log(visitedHouses.size);