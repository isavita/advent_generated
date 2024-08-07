import * as fs from 'fs';

const slopes = [
    { right: 1, down: 1 },
    { right: 3, down: 1 },
    { right: 5, down: 1 },
    { right: 7, down: 1 },
    { right: 1, down: 2 },
];

function countTrees(map: string[], right: number, down: number): number {
    let trees = 0;
    const width = map[0].length;
    for (let row = 0, col = 0; row < map.length; row += down, col = (col + right) % width) {
        if (map[row][col] === '#') {
            trees++;
        }
    }
    return trees;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const map = input.trim().split('\n');

    const treeCounts = slopes.map(slope => countTrees(map, slope.right, slope.down));
    const product = treeCounts.reduce((acc, count) => acc * count, 1);

    console.log(product);
}

main();