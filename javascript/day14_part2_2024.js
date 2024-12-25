
const fs = require('node:fs');

const mod = (a, b) => ((a % b) + b) % b;

const parseLine = (line) => {
    const match = line.match(/p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)/);
    if (!match) throw new Error(`Invalid line format: ${line}`);
    const [, x, y, vx, vy] = match.map(Number);
    return { x, y, vx, vy };
};

const moveRobots = (robots, sizeX, sizeY) => {
    for (const robot of robots) {
        robot.x = mod(robot.x + robot.vx, sizeX);
        robot.y = mod(robot.y + robot.vy, sizeY);
    }
};

const countQuadrants = (robots, sizeX, sizeY) => {
    const counts = [0, 0, 0, 0];
    const centerX = sizeX / 2;
    const centerY = sizeY / 2;

    for (const { x, y } of robots) {
        if (x < centerX) {
            if (y < centerY) counts[0]++;
            else if (y > centerY) counts[1]++;
        } else if (x > centerX) {
            if (y < centerY) counts[2]++;
            else if (y > centerY) counts[3]++;
        }
    }
    return counts;
};

const hasNoOverlaps = (robots) => {
    const positions = new Set();
    for (const { x, y } of robots) {
        const pos = `${x},${y}`;
        if (positions.has(pos)) return false;
        positions.add(pos);
    }
    return true;
};

const drawGrid = (robots, sizeX, sizeY) => {
    const grid = Array(sizeY).fill(null).map(() => Array(sizeX).fill('.'));
    for (const { x, y } of robots) {
        grid[y][x] = '#';
    }
    grid.forEach(row => console.log(row.join('')));
};

const main = () => {
    const sizeX = 101;
    const sizeY = 103;
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const robots = input.filter(line => line).map(parseLine);

    const robotsPart1 = robots.map(r => ({ ...r }));
    for (let n = 0; n < 100; n++) {
        moveRobots(robotsPart1, sizeX, sizeY);
    }
    const counts = countQuadrants(robotsPart1, sizeX, sizeY);
    const safetyFactor = counts.reduce((acc, c) => acc * c, 1);
    console.log(`Part 1 - Safety Factor after 100 seconds: ${safetyFactor}`);

    const robotsPart2 = robots.map(r => ({ ...r }));
    let seconds = 0;
    while (true) {
        if (hasNoOverlaps(robotsPart2)) break;
        moveRobots(robotsPart2, sizeX, sizeY);
        seconds++;
        if (seconds > 1000000) {
            console.log("Exceeded maximum iterations without finding a unique position configuration.");
            process.exit(1);
        }
    }
    console.log(`Part 2 - Fewest seconds to display Easter egg: ${seconds}`);
    console.log("Final positions of robots:");
    drawGrid(robotsPart2, sizeX, sizeY);
};

main();
