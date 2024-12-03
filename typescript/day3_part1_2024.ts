
import * as fs from 'fs';

function solve() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const matches = input.matchAll(/mul\((\d+),(\d+)\)/g);
    let sum = 0;
    for (const match of matches) {
        sum += parseInt(match[1]) * parseInt(match[2]);
    }
    console.log(sum);
}

solve();
