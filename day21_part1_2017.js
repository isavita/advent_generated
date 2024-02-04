const fs = require('fs');

const rules = {};

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
input.forEach(line => {
    const parts = line.split(' => ');
    rules[parts[0]] = parts[1];
});

let grid = [
    ".#.",
    "..#",
    "###",
];

for (let i = 0; i < 5; i++) {
    let newSize;
    let subSize;

    if (grid.length % 2 === 0) {
        subSize = 2;
        newSize = grid.length / 2 * 3;
    } else {
        subSize = 3;
        newSize = grid.length / 3 * 4;
    }

    const newGrid = Array(newSize).fill('');
    
    for (let x = 0; x < newSize; x++) {
        newGrid[x] = '';
    }

    for (let y = 0; y < grid.length; y += subSize) {
        for (let x = 0; x < grid.length; x += subSize) {
            const square = [];
            for (let dy = 0; dy < subSize; dy++) {
                square.push(grid[y + dy].substring(x, x + subSize));
            }
            const newSquare = enhance(square.join('/'), rules);
            newSquare.split('/').forEach((row, dy) => {
                newGrid[y / subSize * (subSize + 1) + dy] += row;
            });
        }
    }
    grid = newGrid;
}

let count = 0;
grid.forEach(row => {
    row.split('').forEach(pixel => {
        if (pixel === '#') {
            count++;
        }
    });
});
console.log(count);

function enhance(input, rules) {
    for (let i = 0; i < 4; i++) {
        if (rules[input]) {
            return rules[input];
        }
        input = rotate(input);
    }
    input = flip(input);
    for (let i = 0; i < 4; i++) {
        if (rules[input]) {
            return rules[input];
        }
        input = rotate(input);
    }
    return '';
}

function rotate(input) {
    const parts = input.split('/');
    const size = parts.length;
    const newParts = Array(size).fill('');
    for (let x = 0; x < size; x++) {
        let newRow = '';
        for (let y = size - 1; y >= 0; y--) {
            newRow += parts[y][x];
        }
        newParts[x] = newRow;
    }
    return newParts.join('/');
}

function flip(input) {
    const parts = input.split('/');
    parts.forEach((part, i) => {
        parts[i] = reverse(part);
    });
    return parts.join('/');
}

function reverse(input) {
    const runes = input.split('');
    for (let i = 0, j = runes.length - 1; i < j; i++, j--) {
        [runes[i], runes[j]] = [runes[j], runes[i]];
    }
    return runes.join('');
}