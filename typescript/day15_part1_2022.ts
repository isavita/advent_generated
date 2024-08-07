import * as fs from 'fs';

interface Point {
    x: number;
    y: number;
}

function parseInput(filePath: string): { sensors: Point[], beacons: Point[] } {
    const data = fs.readFileSync(filePath, 'utf-8');
    const sensors: Point[] = [];
    const beacons: Point[] = [];
    
    const regex = /Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)/g;
    let match;
    
    while ((match = regex.exec(data)) !== null) {
        sensors.push({ x: parseInt(match[1]), y: parseInt(match[2]) });
        beacons.push({ x: parseInt(match[3]), y: parseInt(match[4]) });
    }
    
    return { sensors, beacons };
}

function countUnreachablePositions(sensors: Point[], beacons: Point[], targetY: number): number {
    const unreachable: Set<number> = new Set();
    
    for (const sensor of sensors) {
        const beaconIndex = sensors.indexOf(sensor);
        const beacon = beacons[beaconIndex];
        const distance = Math.abs(sensor.x - beacon.x) + Math.abs(sensor.y - beacon.y);
        
        const verticalDistance = Math.abs(sensor.y - targetY);
        if (verticalDistance < distance) {
            const horizontalCoverage = distance - verticalDistance;
            for (let x = sensor.x - horizontalCoverage; x <= sensor.x + horizontalCoverage; x++) {
                unreachable.add(x);
            }
        }
    }
    
    for (const beacon of beacons) {
        if (beacon.y === targetY) {
            unreachable.delete(beacon.x);
        }
    }
    
    return unreachable.size;
}

const { sensors, beacons } = parseInput('input.txt');
const targetY = 2000000;
const result = countUnreachablePositions(sensors, beacons, targetY);
console.log(result);