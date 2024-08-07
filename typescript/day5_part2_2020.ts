import * as fs from 'fs';

const decodeSeat = (boardingPass: string): number => {
    const rowBinary = boardingPass.slice(0, 7).replace(/F/g, '0').replace(/B/g, '1');
    const colBinary = boardingPass.slice(7).replace(/L/g, '0').replace(/R/g, '1');
    const row = parseInt(rowBinary, 2);
    const col = parseInt(colBinary, 2);
    return row * 8 + col;
};

const findSeatID = (boardingPasses: string[]): number[] => {
    return boardingPasses.map(decodeSeat);
};

const findMissingSeatID = (seatIDs: number[]): number => {
    const allIDs = new Set(seatIDs);
    for (let id = Math.min(...seatIDs); id <= Math.max(...seatIDs); id++) {
        if (!allIDs.has(id)) return id;
    }
    return -1; // In case no missing seat is found
};

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) throw err;
    const boardingPasses = data.trim().split('\n');
    const seatIDs = findSeatID(boardingPasses);
    
    const highestSeatID = Math.max(...seatIDs);
    console.log('Highest Seat ID:', highestSeatID);
    
    const missingSeatID = findMissingSeatID(seatIDs);
    console.log('Missing Seat ID:', missingSeatID);
});