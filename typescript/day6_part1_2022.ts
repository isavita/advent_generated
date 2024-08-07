import * as fs from 'fs';

function findStartOfPacketMarker(datastream: string): number {
    for (let i = 3; i < datastream.length; i++) {
        const lastFourChars = datastream.slice(i - 3, i + 1);
        const uniqueChars = new Set(lastFourChars);
        if (uniqueChars.size === 4) {
            return i + 1;
        }
    }
    return -1; // In case no marker is found
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }
    const position = findStartOfPacketMarker(data.trim());
    console.log(position);
});