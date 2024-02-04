const fs = require('fs');

class Reindeer {
    constructor(speed, flyTime, restTime) {
        this.speed = speed;
        this.flyTime = flyTime;
        this.restTime = restTime;
        this.distance = 0;
        this.points = 0;
        this.flying = true;
        this.timeInMode = 0;
    }
}

function readReindeerDetails(filename) {
    const reindeers = [];
    const data = fs.readFileSync(filename, 'utf8').split('\n');
    for (let i = 0; i < data.length; i++) {
        const parts = data[i].split(' ');
        const speed = parseInt(parts[3]);
        const flyTime = parseInt(parts[6]);
        const restTime = parseInt(parts[13]);
        reindeers.push(new Reindeer(speed, flyTime, restTime));
    }
    return reindeers;
}

function simulateRaceWithPoints(reindeers, totalSeconds) {
    for (let i = 0; i < totalSeconds; i++) {
        let maxDistance = 0;
        for (let j = 0; j < reindeers.length; j++) {
            const reindeer = reindeers[j];
            if (reindeer.flying) {
                reindeer.distance += reindeer.speed;
            }
            reindeer.timeInMode++;
            if ((reindeer.flying && reindeer.timeInMode === reindeer.flyTime) || (!reindeer.flying && reindeer.timeInMode === reindeer.restTime)) {
                reindeer.flying = !reindeer.flying;
                reindeer.timeInMode = 0;
            }
            if (reindeer.distance > maxDistance) {
                maxDistance = reindeer.distance;
            }
        }
        for (let j = 0; j < reindeers.length; j++) {
            const reindeer = reindeers[j];
            if (reindeer.distance === maxDistance) {
                reindeer.points++;
            }
        }
    }
}

function findMaxPoints(reindeers) {
    let maxPoints = 0;
    for (let i = 0; i < reindeers.length; i++) {
        const reindeer = reindeers[i];
        if (reindeer.points > maxPoints) {
            maxPoints = reindeer.points;
        }
    }
    return maxPoints;
}

const reindeers = readReindeerDetails("input.txt");
simulateRaceWithPoints(reindeers, 2503);
const maxPoints = findMaxPoints(reindeers);
console.log(maxPoints);