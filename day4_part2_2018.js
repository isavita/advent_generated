const fs = require('fs');

class Record {
    constructor(time, event) {
        this.time = time;
        this.event = event;
    }
}

class Guard {
    constructor(id) {
        this.id = id;
        this.minutes = new Array(60).fill(0);
        this.totalMin = 0;
    }
}

const records = [];
const guards = {};

const data = fs.readFileSync('input.txt', 'utf8').split('\n');

data.forEach(line => {
    const t = new Date(line.substring(1, 17));
    records.push(new Record(t, line.substring(19)));
});

records.sort((a, b) => a.time - b.time);

let currentGuard;
let sleepStart;

records.forEach(record => {
    if (record.event.includes("begins shift")) {
        const id = parseInt(record.event.split(" ")[1].substring(1));
        if (!guards[id]) {
            guards[id] = new Guard(id);
        }
        currentGuard = guards[id];
    } else if (record.event.includes("falls asleep")) {
        sleepStart = record.time.getMinutes();
    } else if (record.event.includes("wakes up")) {
        for (let i = sleepStart; i < record.time.getMinutes(); i++) {
            currentGuard.minutes[i]++;
            currentGuard.totalMin++;
        }
    }
});

let mostFreqGuard;
let mostFreqMin;

for (const g in guards) {
    for (let i = 0; i < 60; i++) {
        if (!mostFreqGuard || guards[g].minutes[i] > mostFreqGuard.minutes[mostFreqMin]) {
            mostFreqGuard = guards[g];
            mostFreqMin = i;
        }
    }
}

console.log(mostFreqGuard.id * mostFreqMin);