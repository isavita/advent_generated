import * as fs from 'fs';

const findMarkerPosition = (buffer: string, markerLength: number): number => {
    for (let i = markerLength; i <= buffer.length; i++) {
        const chars = buffer.slice(i - markerLength, i);
        const uniqueChars = new Set(chars);
        if (uniqueChars.size === markerLength) {
            return i;
        }
    }
    return -1; // In case no marker is found
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const startOfPacketMarker = findMarkerPosition(input, 4);
    const startOfMessageMarker = findMarkerPosition(input, 14);

    console.log(`First start-of-packet marker after character: ${startOfPacketMarker}`);
    console.log(`First start-of-message marker after character: ${startOfMessageMarker}`);
};

main();