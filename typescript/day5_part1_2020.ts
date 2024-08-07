import * as fs from 'fs';

const decodeSeat = (boardingPass: string): number => {
    const rowBinary = boardingPass.slice(0, 7).replace(/F/g, '0').replace(/B/g, '1');
    const colBinary = boardingPass.slice(7).replace(/L/g, '0').replace(/R/g, '1');
    const row = parseInt(rowBinary, 2);
    const col = parseInt(colBinary, 2);
    return row * 8 + col;
};

const findHighestSeatId = (filePath: string): number => {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    return Math.max(...data.map(decodeSeat));
};

const highestSeatId = findHighestSeatId('input.txt');
console.log(highestSeatId);