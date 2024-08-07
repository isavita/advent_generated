import * as fs from 'fs';

interface Record {
    timestamp: Date;
    action: string;
    guardID: number;
}

const readAndParseInput = (filename: string): Record[] => {
    const data = fs.readFileSync(filename, 'utf-8');
    const lines = data.split('\n');
    const records: Record[] = [];
    const layout = /\[(.+)\] (.+)/;

    for (const line of lines) {
        const match = line.match(layout);
        if (match) {
            const timePart = match[1];
            let actionPart = match[2];
            const ts = new Date(timePart);
            let guardID = -1;

            if (actionPart.includes('Guard')) {
                guardID = parseInt(actionPart.match(/\d+/)![0]);
                actionPart = 'begins shift';
            } else if (actionPart.includes('falls asleep')) {
                actionPart = 'falls asleep';
            } else if (actionPart.includes('wakes up')) {
                actionPart = 'wakes up';
            }

            records.push({ timestamp: ts, action: actionPart, guardID });
        }
    }

    return records;
}

const main = () => {
    const records = readAndParseInput('input.txt');
    records.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());

    const guardSleepMinutes: { [key: number]: number[] } = {};
    let currentGuardID = 0;
    let sleepStart: Date | null = null;

    for (const record of records) {
        switch (record.action) {
            case 'begins shift':
                currentGuardID = record.guardID;
                break;
            case 'falls asleep':
                sleepStart = record.timestamp;
                break;
            case 'wakes up':
                if (!guardSleepMinutes[currentGuardID]) {
                    guardSleepMinutes[currentGuardID] = Array(60).fill(0);
                }
                for (let i = sleepStart!.getMinutes(); i < record.timestamp.getMinutes(); i++) {
                    guardSleepMinutes[currentGuardID][i]++;
                }
                break;
        }
    }

    let maxSleep = 0;
    let sleepiestGuard = 0;
    for (const guardID in guardSleepMinutes) {
        const totalSleep = guardSleepMinutes[guardID].reduce((a, b) => a + b, 0);
        if (totalSleep > maxSleep) {
            maxSleep = totalSleep;
            sleepiestGuard = parseInt(guardID);
        }
    }

    let maxMinute = 0;
    let maxMinuteCount = 0;
    for (let i = 0; i < guardSleepMinutes[sleepiestGuard].length; i++) {
        if (guardSleepMinutes[sleepiestGuard][i] > maxMinuteCount) {
            maxMinuteCount = guardSleepMinutes[sleepiestGuard][i];
            maxMinute = i;
        }
    }

    console.log(sleepiestGuard * maxMinute);
}

main();