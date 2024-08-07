import * as fs from 'fs';

type Row = {
    springs: string;
    group: number[];
};

function parseInput(input: string[]): Row[] {
    return input.map(line => {
        const parts = line.split(' ');
        return {
            springs: parts[0],
            group: parts[1].split(',').map(Number),
        };
    });
}

function countArrangementsRecursive(row: Row, iSprings: number, iGroup: number, iContiguousDamaged: number, cache: Map<string, number>): number {
    if (iSprings === row.springs.length) {
        if (iGroup === row.group.length && iContiguousDamaged === 0) {
            return 1;
        } else if (iGroup === row.group.length - 1 && iContiguousDamaged === row.group[iGroup]) {
            return 1;
        }
        return 0;
    }

    const cacheKey = `${iSprings},${iGroup},${iContiguousDamaged}`;
    if (cache.has(cacheKey)) {
        return cache.get(cacheKey)!;
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

function countArrangements(row: Row): number {
    return countArrangementsRecursive(row, 0, 0, 0, new Map());
}

function unfoldRow(row: Row, unfoldingFactor: number): Row {
    let newSprings = row.springs;
    let newGroup = [...row.group];

    for (let i = 1; i < unfoldingFactor; i++) {
        newSprings += '?' + row.springs;
        newGroup = newGroup.concat(row.group);
    }

    return { springs: newSprings, group: newGroup };
}

function solve(input: string[]): number {
    const rows = parseInput(input);
    const unfoldedRows = rows.map(row => unfoldRow(row, 5));

    return unfoldedRows.reduce((acc, row) => acc + countArrangements(row), 0);
}

function readFile(fileName: string): string[] {
    return fs.readFileSync(fileName).toString().trim().split('\n');
}

const input = readFile('input.txt');
console.log(solve(input));