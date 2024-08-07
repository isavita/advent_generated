import * as fs from 'fs';
import * as path from 'path';

type DistanceMap = { [key: string]: { [key: string]: number } };
type Route = string[];

function parseInput(input: string): DistanceMap {
    const distances: DistanceMap = {};
    input.split('\n').forEach(line => {
        const [, from, to, distance] = line.match(/^(\w+) to (\w+) = (\d+)$/)!;
        if (!distances[from]) distances[from] = {};
        if (!distances[to]) distances[to] = {};
        distances[from][to] = parseInt(distance, 10);
        distances[to][from] = parseInt(distance, 10);
    });
    return distances;
}

function calculateRouteDistance(route: Route, distances: DistanceMap): number {
    return route.slice(0, -1).reduce((total, city, index) => {
        return total + distances[city][route[index + 1]];
    }, 0);
}

function findRoutes(cities: string[], distances: DistanceMap): Route[] {
    const routes: Route[] = [];
    const permute = (arr: string[], m: string[] = []) => {
        if (arr.length === 0) {
            routes.push(m);
        } else {
            for (let i = 0; i < arr.length; i++) {
                const currPos = arr.slice();
                const nextPos = currPos.splice(i, 1);
                permute(currPos.slice(), m.concat(nextPos));
            }
        }
    };
    permute(cities);
    return routes;
}

function main() {
    const inputPath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(inputPath, 'utf-8').trim();
    const distances = parseInput(input);
    const cities = Object.keys(distances);
    const routes = findRoutes(cities, distances);

    let shortestRoute = Number.MAX_SAFE_INTEGER;
    let longestRoute = 0;

    for (const route of routes) {
        const distance = calculateRouteDistance(route, distances);
        if (distance < shortestRoute) {
            shortestRoute = distance;
        }
        if (distance > longestRoute) {
            longestRoute = distance;
        }
    }

    console.log(`Shortest route distance: ${shortestRoute}`);
    console.log(`Longest route distance: ${longestRoute}`);
}

main();