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
    const fileContent = fs.readFileSync(filename, 'utf8');
    const lines = fileContent.trim().split('\n');
    lines.forEach(line => {
        const parts = line.split(' ');
        const speed = parseInt(parts[3]);
        const flyTime = parseInt(parts[6]);
        const restTime = parseInt(parts[13]);
        reindeers.push(new Reindeer(speed, flyTime, restTime));
    });
    return reindeers;
}

function simulateRaceWithPoints(reindeers, totalSeconds) {
    for (let i = 0; i < totalSeconds; i++) {
        let maxDistance = 0;
        reindeers.forEach(reindeer => {
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
        });
        reindeers.forEach(reindeer => {
            if (reindeer.distance === maxDistance) {
                reindeer.points++;
            }
        });
    }
}

function findMaxPoints(reindeers) {
    let maxPoints = 0;
    reindeers.forEach(reindeer => {
        if (reindeer.points > maxPoints) {
            maxPoints = reindeer.points;
        }
    });
    return maxPoints;
}

const reindeers = readReindeerDetails('input.txt');
simulateRaceWithPoints(reindeers, 2503);
const maxPoints = findMaxPoints(reindeers);
console.log(maxPoints);