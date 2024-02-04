const fs = require('fs');

function abs(x) {
    if (x < 0) {
        return -x;
    }
    return x;
}

function max(a, b) {
    if (a > b) {
        return a;
    }
    return b;
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

        // Calculate the current distance
        const curDistance = distance(x, y, z);
        maxDistance = max(maxDistance, curDistance);
    }

    console.log(maxDistance);
});