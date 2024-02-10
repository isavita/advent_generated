const fs = require('fs');

const data = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const seatIDs = data.map(pass => {
    pass = pass.replace(/F/g, '0').replace(/B/g, '1').replace(/L/g, '0').replace(/R/g, '1');
    return decode(pass);
});

seatIDs.sort((a, b) => a - b);

for (let i = 0; i < seatIDs.length - 1; i++) {
    if (seatIDs[i + 1] !== seatIDs[i] + 1) {
        console.log(seatIDs[i] + 1);
        break;
    }
}

function decode(pass) {
    const row = binaryToInt(pass.slice(0, 7));
    const column = binaryToInt(pass.slice(7));
    return row * 8 + column;
}

function binaryToInt(binaryStr) {
    let result = 0;
    for (let i = 0; i < binaryStr.length; i++) {
        if (binaryStr[i] === '1') {
            result |= 1 << (binaryStr.length - i - 1);
        }
    }
    return result;
}