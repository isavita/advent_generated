const fs = require('fs');

class Record {
    constructor(timestamp, action, guardID) {
        this.timestamp = timestamp;
        this.action = action;
        this.guardID = guardID;
    }
}

function readAndParseInput(filename) {
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    const records = [];
    const layout = "2006-01-02 15:04";

    data.forEach(line => {
        const parts = line.split("] ");
        const timePart = parts[0].slice(1);
        const actionPart = parts[1];

        const ts = new Date(timePart);
        let guardID = -1;

        if (actionPart.includes("Guard")) {
            guardID = parseInt(actionPart.match(/\d+/)[0]);
            records.push(new Record(ts, "begins shift", guardID));
        } else if (actionPart.includes("falls asleep")) {
            records.push(new Record(ts, "falls asleep", -1));
        } else if (actionPart.includes("wakes up")) {
            records.push(new Record(ts, "wakes up", -1));
        }
    });

    return records;
}

const records = readAndParseInput("input.txt");
records.sort((a, b) => a.timestamp - b.timestamp);

const guardSleepMinutes = {};
let currentGuardID = 0;
let sleepStart = null;

records.forEach(record => {
    switch (record.action) {
        case "begins shift":
            currentGuardID = record.guardID;
            break;
        case "falls asleep":
            sleepStart = record.timestamp;
            break;
        case "wakes up":
            if (!guardSleepMinutes[currentGuardID]) {
                guardSleepMinutes[currentGuardID] = new Array(60).fill(0);
            }
            for (let i = sleepStart.getMinutes(); i < record.timestamp.getMinutes(); i++) {
                guardSleepMinutes[currentGuardID][i]++;
            }
            break;
    }
});

let maxSleep = 0;
let sleepiestGuard = 0;

Object.entries(guardSleepMinutes).forEach(([guardID, minutes]) => {
    let totalSleep = 0;
    minutes.forEach(count => {
        totalSleep += count;
    });
    if (totalSleep > maxSleep) {
        maxSleep = totalSleep;
        sleepiestGuard = guardID;
    }
});

let maxMinute = 0;
let maxMinuteCount = 0;

guardSleepMinutes[sleepiestGuard].forEach((count, i) => {
    if (count > maxMinuteCount) {
        maxMinuteCount = count;
        maxMinute = i;
    }
});

console.log(sleepiestGuard * maxMinute);