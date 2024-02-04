const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

class Row {
    constructor(springs, group) {
        this.springs = springs;
        this.group = group;
    }
}

function parseInput(input) {
    const rows = [];
    for (const line of input) {
        const parts = line.split(' ');
        const springs = parts[0];
        const ints = parseStringToInts(parts[1]);

        const row = new Row(springs, ints);
        rows.push(row);
    }
    return rows;
}

function parseStringToInts(numbersLine) {
    const numbers = [];
    const numbersParts = numbersLine.split(',');
    for (const numberStr of numbersParts) {
        const number = parseInt(numberStr, 10);
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

    const cacheKey = [iSprings, iGroup, iContiguousDamaged].toString();
    if (cache.has(cacheKey)) {
        return cache.get(cacheKey);
    }

    let res = 0;
    const char = row.springs[iSprings];
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

    cache.set(cacheKey, res);
    return res;
}

function countArrangements(row) {
    return countArrangementsRecursive(row, 0, 0, 0, new Map());
}

function unfoldRow(row, unfoldingFactor) {
    const newRow = new Row(row.springs, row.group.slice());

    for (let i = 1; i < unfoldingFactor; i++) {
        newRow.springs += '?' + row.springs;
        newRow.group.push(...row.group);
    }

    return newRow;
}

function solve(input) {
    const rows = parseInput(input);

    let res = 0;
    for (const row of rows) {
        res += countArrangements(row);
    }

    return res;
}

console.log(solve(input));