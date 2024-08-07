import * as fs from 'fs';
import * as readline from 'readline';

interface DistanceMap {
    [key: string]: { [key: string]: number };
}

function calculateTotalDistance(route: string[], distanceMap: DistanceMap): number {
    let totalDistance = 0;
    for (let i = 0; i < route.length - 1; i++) {
        const from = route[i];
        const to = route[i + 1];
        totalDistance += distanceMap[from][to];
    }
    return totalDistance;
}

function findShortestRoute(locations: string[], distanceMap: DistanceMap): number {
    let shortestDistance = Infinity;

    function permute(arr: string[], m: number = 0): void {
        if (m === arr.length - 1) {
            const distance = calculateTotalDistance(arr, distanceMap);
            if (distance < shortestDistance) {
                shortestDistance = distance;
            }
        } else {
            for (let i = m; i < arr.length; i++) {
                [arr[m], arr[i]] = [arr[i], arr[m]];
                permute(arr, m + 1);
                [arr[m], arr[i]] = [arr[i], arr[m]];
            }
        }
    }

    permute(locations.slice());
    return shortestDistance;
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const distanceMap: DistanceMap = {};
    const locations = new Set<string>();

    for await (const line of rl) {
        const [from, , to, , distanceStr] = line.split(' ');
        const distance = parseInt(distanceStr, 10);
        if (!distanceMap[from]) {
            distanceMap[from] = {};
        }
        if (!distanceMap[to]) {
            distanceMap[to] = {};
        }
        distanceMap[from][to] = distance;
        distanceMap[to][from] = distance;
        locations.add(from);
        locations.add(to);
    }

    const shortestDistance = findShortestRoute(Array.from(locations), distanceMap);
    console.log(shortestDistance);
}

main().catch(console.error);