import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');

let x = 1;
let cycle = 0;
const signalStrengths: number[] = [];
const crt: string[][] = Array.from({ length: 6 }, () => Array(40).fill('.'));

const recordSignalStrength = () => {
    if ((cycle - 20) % 40 === 0) {
        signalStrengths.push(cycle * x);
    }
};

const drawPixel = () => {
    const row = Math.floor(cycle / 40);
    const col = cycle % 40;
    if (col >= x - 1 && col <= x + 1) {
        crt[row][col] = '#';
    }
};

for (const line of input) {
    const [cmd, arg] = line.split(' ');
    if (cmd === 'noop') {
        drawPixel();
        cycle++;
        recordSignalStrength();
    } else if (cmd === 'addx') {
        drawPixel();
        cycle++;
        recordSignalStrength();
        drawPixel();
        cycle++;
        recordSignalStrength();
        x += Number(arg);
    }
}

console.log('Signal Strength Sum:', signalStrengths.reduce((sum, val) => sum + val, 0));
console.log('CRT Output:');
crt.forEach(row => console.log(row.join('')));