import * as fs from 'fs';

type Point = [number, number];
type Cave = Map<string, string>;

const parseInput = (data: string): Point[][] => {
    return data.trim().split('\n').map(line => 
        line.split(' -> ').map(point => point.split(',').map(Number) as Point)
    );
};

const drawCave = (paths: Point[][]): Cave => {
    const cave: Cave = new Map();
    paths.forEach(path => {
        for (let i = 0; i < path.length - 1; i++) {
            const [x1, y1] = path[i];
            const [x2, y2] = path[i + 1];
            const dx = x2 - x1 === 0 ? 0 : (x2 - x1 > 0 ? 1 : -1);
            const dy = y2 - y1 === 0 ? 0 : (y2 - y1 > 0 ? 1 : -1);
            let x = x1, y = y1;
            while (x !== x2 || y !== y2) {
                cave.set(`${x},${y}`, '#');
                if (x !== x2) x += dx;
                if (y !== y2) y += dy;
            }
            cave.set(`${x2},${y2}`, '#');
        }
    });
    return cave;
};

const simulateSand = (cave: Cave): number => {
    let sandCount = 0;
    const source: Point = [500, 0];

    while (true) {
        let [x, y] = source;
        let sandFell = false;

        while (true) {
            if (!cave.has(`${x},${y + 1}`)) {
                y++;
                sandFell = true;
            } else if (!cave.has(`${x - 1},${y + 1}`)) {
                x--;
                y++;
                sandFell = true;
            } else if (!cave.has(`${x + 1},${y + 1}`)) {
                x++;
                y++;
                sandFell = true;
            } else {
                break;
            }

            if (y >= 1000) return sandCount; // Exit if sand falls into the abyss
        }
        
        cave.set(`${x},${y}`, 'o');
        sandCount++;

        if (!sandFell) break; // Exit if no sand fell
    }
    return sandCount;
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const paths = parseInput(input);
    const cave = drawCave(paths);
    const result = simulateSand(cave);
    console.log(result);
};

main();