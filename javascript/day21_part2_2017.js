const fs = require('fs');

const memo = {};

function enhance(input, rules) {
    if (memo[input]) {
        return memo[input];
    }

    const original = input;
    for (let i = 0; i < 4; i++) {
        if (rules[input]) {
            memo[original] = rules[input];
            return rules[input];
        }
        input = rotate(input);
    }
    input = flip(input);
    for (let i = 0; i < 4; i++) {
        if (rules[input]) {
            memo[original] = rules[input];
            return rules[input];
        }
        input = rotate(input);
    }
    return "";
}

function rotate(input) {
    const parts = input.split("/");
    const size = parts.length;
    const newParts = [];
    for (let x = 0; x < size; x++) {
        let newRow = "";
        for (let y = size - 1; y >= 0; y--) {
            newRow += parts[y][x];
        }
        newParts.push(newRow);
    }
    return newParts.join("/");
}

function flip(input) {
    const parts = input.split("/");
    for (let i = 0; i < parts.length; i++) {
        parts[i] = reverse(parts[i]);
    }
    return parts.join("/");
}

function reverse(input) {
    return input.split("").reverse().join("");
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const rules = {};
    const lines = data.split('\n');
    for (const line of lines) {
        const parts = line.split(' => ');
        rules[parts[0]] = parts[1];
    }

    let grid = [
        ".#.",
        "..#",
        "###",
    ];

    for (let i = 0; i < 18; i++) {
        let newSize;
        let subSize;

        if (grid.length % 2 === 0) {
            subSize = 2;
            newSize = grid.length / 2 * 3;
        } else {
            subSize = 3;
            newSize = grid.length / 3 * 4;
        }

        const newGrid = Array(newSize).fill("");

        for (let y = 0; y < grid.length; y += subSize) {
            for (let x = 0; x < grid.length; x += subSize) {
                const square = [];
                for (let dy = 0; dy < subSize; dy++) {
                    square.push(grid[y + dy].substring(x, x + subSize));
                }
                const newSquare = enhance(square.join("/"), rules);
                const rows = newSquare.split("/");
                for (let dy = 0; dy < rows.length; dy++) {
                    newGrid[y / subSize * (subSize + 1) + dy] += rows[dy];
                }
            }
        }
        grid = newGrid;
    }

    let count = 0;
    for (const row of grid) {
        for (const pixel of row) {
            if (pixel === '#') {
                count++;
            }
        }
    }
    console.log(count);
});