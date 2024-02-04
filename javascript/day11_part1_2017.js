const fs = require('fs');

function abs(x) {
    return x < 0 ? -x : x;
}

function max(a, b) {
    return a > b ? a : b;
}

function distance(x, y, z) {
    return (abs(x) + abs(y) + abs(z)) / 2;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("File reading error", err);
        return;
    }

    const directions = data.trim().split(',');

    let x = 0, y = 0, z = 0;
    let maxDistance = 0;

    for (let dir of directions) {
        switch (dir) {
            case "n":
                y++;
                z--;
                break;
            case "ne":
                x++;
                z--;
                break;
            case "se":
                x++;
                y--;
                break;
            case "s":
                y--;
                z++;
                break;
            case "sw":
                x--;
                z++;
                break;
            case "nw":
                x--;
                y++;
                break;
        }

        let curDistance = distance(x, y, z);
        maxDistance = max(maxDistance, curDistance);
    }

    console.log(distance(x, y, z));
});