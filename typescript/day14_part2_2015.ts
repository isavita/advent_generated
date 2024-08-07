import * as fs from 'fs';

interface Reindeer {
    name: string;
    speed: number;
    flyTime: number;
    restTime: number;
}

function parseInput(input: string): Reindeer[] {
    const reindeerList: Reindeer[] = [];
    const lines = input.trim().split('\n');

    for (const line of lines) {
        const match = line.match(/(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds/);
        if (match) {
            const [, name, speed, flyTime, restTime] = match;
            reindeerList.push({
                name,
                speed: parseInt(speed, 10),
                flyTime: parseInt(flyTime, 10),
                restTime: parseInt(restTime, 10),
            });
        }
    }

    return reindeerList;
}

function simulateRace(reindeerList: Reindeer[], duration: number): number {
    let maxDistance = 0;

    for (const reindeer of reindeerList) {
        const cycleTime = reindeer.flyTime + reindeer.restTime;
        const fullCycles = Math.floor(duration / cycleTime);
        const remainingTime = duration % cycleTime;
        const distance = (fullCycles * reindeer.flyTime + Math.min(remainingTime, reindeer.flyTime)) * reindeer.speed;

        maxDistance = Math.max(maxDistance, distance);
    }

    return maxDistance;
}

function calculatePoints(reindeerList: Reindeer[], duration: number): number {
    const points: { [key: string]: number } = {};
    const distances: { [key: string]: number } = {};

    for (const reindeer of reindeerList) {
        points[reindeer.name] = 0;
        distances[reindeer.name] = 0;
    }

    for (let time = 1; time <= duration; time++) {
        let maxDistance = 0;
        let leaders: string[] = [];

        for (const reindeer of reindeerList) {
            const cycleTime = reindeer.flyTime + reindeer.restTime;
            const fullCycles = Math.floor(time / cycleTime);
            const remainingTime = time % cycleTime;
            const distance = (fullCycles * reindeer.flyTime + Math.min(remainingTime, reindeer.flyTime)) * reindeer.speed;

            distances[reindeer.name] = distance;
            if (distance > maxDistance) {
                maxDistance = distance;
                leaders = [reindeer.name];
            } else if (distance === maxDistance) {
                leaders.push(reindeer.name);
            }
        }

        for (const leader of leaders) {
            points[leader]++;
        }
    }

    let maxPoints = 0;
    for (const reindeer of reindeerList) {
        maxPoints = Math.max(maxPoints, points[reindeer.name]);
    }

    return maxPoints;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const reindeerList = parseInput(input);
    const raceDuration = 2503;

    const winningDistance = simulateRace(reindeerList, raceDuration);
    const winningPoints = calculatePoints(reindeerList, raceDuration);

    console.log(`The winning reindeer traveled ${winningDistance} km.`);
    console.log(`The winning reindeer has ${winningPoints} points.`);
}

main();