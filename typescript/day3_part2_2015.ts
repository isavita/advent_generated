import * as fs from 'fs';

type Coordinate = [number, number];

function readInput(filePath: string): string {
    return fs.readFileSync(filePath, 'utf8').trim();
}

function getHousesVisited(directions: string): Set<string> {
    const houses = new Set<string>();
    let position: Coordinate = [0, 0];
    houses.add(position.toString());

    for (const direction of directions) {
        switch (direction) {
            case '^': position[1]++; break;
            case 'v': position[1]--; break;
            case '>': position[0]++; break;
            case '<': position[0]--; break;
        }
        houses.add(position.toString());
    }

    return houses;
}

function getHousesVisitedWithRobo(directions: string): Set<string> {
    const houses = new Set<string>();
    let santaPosition: Coordinate = [0, 0];
    let roboSantaPosition: Coordinate = [0, 0];
    houses.add(santaPosition.toString());

    let isSantaTurn = true;
    for (const direction of directions) {
        if (isSantaTurn) {
            switch (direction) {
                case '^': santaPosition[1]++; break;
                case 'v': santaPosition[1]--; break;
                case '>': santaPosition[0]++; break;
                case '<': santaPosition[0]--; break;
            }
            houses.add(santaPosition.toString());
        } else {
            switch (direction) {
                case '^': roboSantaPosition[1]++; break;
                case 'v': roboSantaPosition[1]--; break;
                case '>': roboSantaPosition[0]++; break;
                case '<': roboSantaPosition[0]--; break;
            }
            houses.add(roboSantaPosition.toString());
        }
        isSantaTurn = !isSantaTurn;
    }

    return houses;
}

function main() {
    const input = readInput('input.txt');

    const housesVisited = getHousesVisited(input);
    console.log(`Part One: ${housesVisited.size} houses receive at least one present.`);

    const housesVisitedWithRobo = getHousesVisitedWithRobo(input);
    console.log(`Part Two: ${housesVisitedWithRobo.size} houses receive at least one present.`);
}

main();