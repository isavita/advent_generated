const fs = require('fs');

const directions = {
    "e": { q: 1, r: 0 },
    "se": { q: 0, r: 1 },
    "sw": { q: -1, r: 1 },
    "w": { q: -1, r: 0 },
    "nw": { q: 0, r: -1 },
    "ne": { q: 1, r: -1 },
};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const blackTiles = new Map();
    const lines = data.trim().split('\n');

    lines.forEach(line => {
        let coord = { q: 0, r: 0 };

        for (let i = 0; i < line.length; i++) {
            let dir;
            switch (line[i]) {
                case 'e':
                case 'w':
                    dir = line[i];
                    break;
                case 'n':
                case 's':
                    dir = line[i] + line[i + 1];
                    i++;
                    break;
            }
            const move = directions[dir];
            coord.q += move.q;
            coord.r += move.r;
        }

        const key = `${coord.q},${coord.r}`;
        blackTiles.set(key, !blackTiles.get(key));
    });

    let count = 0;
    for (const black of blackTiles.values()) {
        if (black) {
            count++;
        }
    }
    console.log(count);
});