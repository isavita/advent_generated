import * as fs from 'fs';
import * as readline from 'readline';

interface Reindeer {
    name: string;
    speed: number;
    flyTime: number;
    restTime: number;
}

function parseReindeer(line: string): Reindeer {
    const match = line.match(/(\w+) can fly (\d+) km\/s for (\d+) seconds, but then must rest for (\d+) seconds./);
    if (!match) {
        throw new Error(`Invalid input line: ${line}`);
    }
    return {
        name: match[1],
        speed: parseInt(match[2], 10),
        flyTime: parseInt(match[3], 10),
        restTime: parseInt(match[4], 10),
    };
}

function calculateDistance(reindeer: Reindeer, time: number): number {
    const cycleTime = reindeer.flyTime + reindeer.restTime;
    const fullCycles = Math.floor(time / cycleTime);
    const remainingTime = time % cycleTime;

    let distance = fullCycles * reindeer.flyTime * reindeer.speed;
    if (remainingTime > reindeer.flyTime) {
        distance += reindeer.flyTime * reindeer.speed;
    } else {
        distance += remainingTime * reindeer.speed;
    }

    return distance;
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const reindeers: Reindeer[] = [];

    for await (const line of rl) {
        reindeers.push(parseReindeer(line));
    }

    const raceTime = 2503;
    let maxDistance = 0;

    for (const reindeer of reindeers) {
        const distance = calculateDistance(reindeer, raceTime);
        if (distance > maxDistance) {
            maxDistance = distance;
        }
    }

    console.log(`The winning reindeer has traveled ${maxDistance} km after ${raceTime} seconds.`);
}

main().catch(console.error);