const fs = require('fs');

class Reindeer {
    constructor(speed, flyTime, restTime) {
        this.speed = speed;
        this.flyTime = flyTime;
        this.restTime = restTime;
        this.distance = 0;
        this.flying = true;
        this.timeInMode = 0;
    }
}

function readReindeerDetails(filename) {
    const reindeers = [];
    const input = fs.readFileSync(filename, 'utf8').split('\n');
    for (let line of input) {
        const parts = line.split(' ');
        const speed = parseInt(parts[3]);
        const flyTime = parseInt(parts[6]);
        const restTime = parseInt(parts[13]);
        reindeers.push(new Reindeer(speed, flyTime, restTime));
    }
    return reindeers;
}

function simulateRace(reindeers, totalSeconds) {
    for (let i = 0; i < totalSeconds; i++) {
        for (let j = 0; j < reindeers.length; j++) {
            const reindeer = reindeers[j];
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed;
                reindeer.timeInMode++;
                if (reindeer.timeInMode === reindeer.flyTime) {
                    reindeer.flying = false;
                    reindeer.timeInMode = 0;
                }
            } else {
                reindeer.timeInMode++;
                if (reindeer.timeInMode === reindeer.restTime) {
                    reindeer.flying = true;
                    reindeer.timeInMode = 0;
                }
            }
        }
    }
}

function findMaxDistance(reindeers) {
    let maxDistance = 0;
    for (let reindeer of reindeers) {
        if (reindeer.distance > maxDistance) {
            maxDistance = reindeer.distance;
        }
    }
    return maxDistance;
}

const reindeers = readReindeerDetails("input.txt");
simulateRace(reindeers, 2503);
const maxDistance = findMaxDistance(reindeers);
console.log(maxDistance);