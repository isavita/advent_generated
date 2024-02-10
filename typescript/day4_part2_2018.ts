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
    this.minutes = Array(60).fill(0);
    this.totalMin = 0;
  }
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const records = [];
const guards = {};

input.forEach((line) => {
  const t = new Date(line.substring(1, 17));
  records.push(new Record(t, line.substring(19)));
});

records.sort((a, b) => a.time - b.time);

let currentGuard;
let sleepStart;

records.forEach((record) => {
  switch (true) {
    case record.event.includes('begins shift'):
      const id = parseInt(record.event.split(' ')[1].substring(1));
      if (!guards[id]) {
        guards[id] = new Guard(id);
      }
      currentGuard = guards[id];
      break;
    case record.event.includes('falls asleep'):
      sleepStart = record.time.getMinutes();
      break;
    case record.event.includes('wakes up'):
      for (let i = sleepStart; i < record.time.getMinutes(); i++) {
        currentGuard.minutes[i]++;
        currentGuard.totalMin++;
      }
      break;
  }
});

let mostFreqGuard;
let mostFreqMin;

Object.values(guards).forEach((g) => {
  g.minutes.forEach((m, i) => {
    if (!mostFreqGuard || m > mostFreqGuard.minutes[mostFreqMin]) {
      mostFreqGuard = g;
      mostFreqMin = i;
    }
  });
});

console.log(mostFreqGuard.id * mostFreqMin);