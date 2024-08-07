import * as fs from 'fs';

function calculateWaysToWinLongRace(time: number, record: number): number {
    let waysToWin = 0;
    for (let holdTime = 1; holdTime < time; holdTime++) {
        let travelTime = time - holdTime;
        let distance = holdTime * travelTime;
        if (distance > record) {
            waysToWin++;
        }
    }
    return waysToWin;
}

fs.readFile('input.txt', 'utf8', (err: NodeJS.ErrnoException | null, data: string) => {
    if (err) {
        console.error(err);
        return;
    }
    const lines = data.split('\n');
    let time: number | null = null;
    let distance: number | null = null;
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i].trim();
        if (!line) continue;
        const parts = line.split(':');
        const num = parseInt(parts[1].replace(/\s+/g, ''));
        if (time === null) {
            time = num;
        } else {
            distance = num;
        }
    }
    if (time !== null && distance !== null) {
        const waysToWin = calculateWaysToWinLongRace(time, distance);
        console.log(waysToWin);
    } else {
        console.error('Invalid input file format');
    }
});