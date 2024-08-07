import * as fs from 'fs';

const countFullyContainedPairs = (input: string): number => {
    return input.split('\n').reduce((count, line) => {
        const [[start1, end1], [start2, end2]] = line.split(',').map(range => range.split('-').map(Number));
        return count + (start1 <= start2 && end1 >= end2 || start2 <= start1 && end2 >= end1 ? 1 : 0);
    }, 0);
};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) throw err;
    const result = countFullyContainedPairs(data.trim());
    console.log(result);
});