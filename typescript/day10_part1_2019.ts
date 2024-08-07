import * as fs from 'fs';
import * as path from 'path';

const gcd = (a: number, b: number): number => {
    return b === 0 ? Math.abs(a) : gcd(b, a % b);
};

const countVisibleAsteroids = (map: string[][], x: number, y: number): number => {
    const angles = new Set<string>();
    const height = map.length;
    const width = map[0].length;

    for (let i = 0; i < height; i++) {
        for (let j = 0; j < width; j++) {
            if (map[i][j] === '#' && (i !== y || j !== x)) {
                const dx = j - x;
                const dy = i - y;
                const divisor = gcd(dx, dy);
                const angle = `${dy / divisor},${dx / divisor}`;
                angles.add(angle);
            }
        }
    }
    return angles.size;
};

const findBestLocation = (map: string[][]): { x: number; y: number; count: number } => {
    let bestLocation = { x: -1, y: -1, count: 0 };

    for (let y = 0; y < map.length; y++) {
        for (let x = 0; x < map[0].length; x++) {
            if (map[y][x] === '#') {
                const count = countVisibleAsteroids(map, x, y);
                if (count > bestLocation.count) {
                    bestLocation = { x, y, count };
                }
            }
        }
    }
    return bestLocation;
};

const main = () => {
    const filePath = path.join(__dirname, 'input.txt');
    const input = fs.readFileSync(filePath, 'utf-8');
    const map = input.trim().split('\n').map(line => line.split(''));

    const bestLocation = findBestLocation(map);
    console.log(`Best location: (${bestLocation.x}, ${bestLocation.y}) can detect ${bestLocation.count} other asteroids.`);
};

main();