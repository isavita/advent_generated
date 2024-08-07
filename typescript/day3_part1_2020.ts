import * as fs from 'fs';

const countTrees = (map: string[], right: number, down: number): number => {
    let trees = 0;
    const width = map[0].length;
    for (let row = 0, col = 0; row < map.length; row += down, col = (col + right) % width) {
        if (map[row][col] === '#') {
            trees++;
        }
    }
    return trees;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const map = input.trim().split('\n');
    const result = countTrees(map, 3, 1);
    console.log(result);
};

main();