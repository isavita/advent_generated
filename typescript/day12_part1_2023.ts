const fs = require('fs');

class Row {
    constructor(springs, group) {
        this.springs = springs;
        this.group = group;
    }
}

function parseInput(input) {
    let rows = [];
    for (let line of input) {
        let parts = line.split(" ");
        let springs = parts[0];
        let ints = parseStringToInts(parts[1]);

        let row = new Row(springs, ints);
        rows.push(row);
    }
    return rows;
}

function parseStringToInts(numbersLine) {
    let numbers = [];
    let numbersParts = numbersLine.split(",");
    for (let numberStr of numbersParts) {
        let number = parseInt(numberStr);
        numbers.push(number);
    }
    return numbers;
}

function countArrangementsRecursive(row, iSprings, iGroup, iContiguousDamaged, cache) {
    if (iSprings === row.springs.length) {
        if (iGroup === row.group.length && iContiguousDamaged === 0) {
            return 1;
        } else if (iGroup === row.group.length - 1 && iContiguousDamaged === row.group[iGroup]) {
            return 1;
        }
        return 0;
    }

    let cacheKey = [iSprings, iGroup, iContiguousDamaged];
    if (cache[cacheKey]) {
        return cache[cacheKey];
    }

    let res = 0;
    let char = row.springs[iSprings];
    if (char === '.' || char === '?') {
        if (iContiguousDamaged === 0) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged, cache);
        } else if (iContiguousDamaged === row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup + 1, 0, cache);
        }
    }
    if (char === '#' || char === '?') {
        if (iGroup < row.group.length && iContiguousDamaged < row.group[iGroup]) {
            res += countArrangementsRecursive(row, iSprings + 1, iGroup, iContiguousDamaged + 1, cache);
        }
    }

    cache[cacheKey] = res;
    return res;
}

function countArrangements(row) {
    return countArrangementsRecursive(row, 0, 0, 0, {});
}

function unfoldRow(row, unfoldingFactor) {
    let newRow = new Row(row.springs, row.group);

    for (let i = 1; i < unfoldingFactor; i++) {
        newRow.springs += "?" + row.springs;
        newRow.group.push(...row.group);
    }

    return newRow;
}

function solve(input) {
    let rows = parseInput(input);

    let res = 0;
    for (let row of rows) {
        res += countArrangements(row);
    }

    return res;
}

function readFile(fileName) {
    let file = fs.readFileSync(fileName, 'utf8');
    return file.trim().split("\n");
}

let input = readFile("input.txt");
console.log(solve(input));