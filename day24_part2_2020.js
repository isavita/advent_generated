let fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

let tiles = new Set();
for (let line of input) {
    let x = 0, y = 0, z = 0;
    let i = 0;
    while (i < line.length) {
        switch (line[i]) {
            case 'e':
                x++;
                y--;
                break;
            case 'w':
                x--;
                y++;
                break;
            case 'n':
                i++;
                if (line[i] === 'e') {
                    x++;
                    z--;
                } else {
                    y++;
                    z--;
                }
                break;
            case 's':
                i++;
                if (line[i] === 'e') {
                    y--;
                    z++;
                } else {
                    x--;
                    z++;
                }
                break;
        }
        i++;
    }
    let key = `${x},${y},${z}`;
    if (tiles.has(key)) {
        tiles.delete(key);
    } else {
        tiles.add(key);
    }
}

const getNeighbors = (x, y, z) => {
    return [
        [x + 1, y - 1, z],
        [x - 1, y + 1, z],
        [x + 1, y, z - 1],
        [x, y + 1, z - 1],
        [x, y - 1, z + 1],
        [x - 1, y, z + 1]
    ];
}

const countAdjacentBlack = (x, y, z) => {
    let count = 0;
    let neighbors = getNeighbors(x, y, z);
    for (let neighbor of neighbors) {
        let key = `${neighbor[0]},${neighbor[1]},${neighbor[2]}`;
        if (tiles.has(key)) {
            count++;
        }
    }
    return count;
}

for (let i = 0; i < 100; i++) {
    let newTiles = new Set();
    let whiteTiles = new Set();
    for (let tile of tiles) {
        let [x, y, z] = tile.split(',').map(Number);
        let count = countAdjacentBlack(x, y, z);
        if (count === 1 || count === 2) {
            newTiles.add(tile);
        }
        let neighbors = getNeighbors(x, y, z);
        for (let neighbor of neighbors) {
            let key = `${neighbor[0]},${neighbor[1]},${neighbor[2]}`;
            if (!tiles.has(key) && !whiteTiles.has(key)) {
                count = countAdjacentBlack(neighbor[0], neighbor[1], neighbor[2]);
                if (count === 2) {
                    newTiles.add(key);
                } else {
                    whiteTiles.add(key);
                }
            }
        }
    }
    tiles = newTiles;
}

console.log(tiles.size);