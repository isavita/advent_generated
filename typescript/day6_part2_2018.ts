import * as fs from 'fs';

type Coordinate = [number, number];

const readCoordinates = (filePath: string): Coordinate[] => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').map(line => line.split(', ').map(Number) as Coordinate);
};

const manhattanDistance = (a: Coordinate, b: Coordinate): number => {
    return Math.abs(a[0] - b[0]) + Math.abs(a[1] - b[1]);
};

const largestFiniteArea = (coordinates: Coordinate[]): number => {
    const minX = Math.min(...coordinates.map(c => c[0]));
    const maxX = Math.max(...coordinates.map(c => c[0]));
    const minY = Math.min(...coordinates.map(c => c[1]));
    const maxY = Math.max(...coordinates.map(c => c[1]));
    
    const areaCount = new Map<number, number>();
    const infiniteAreas = new Set<number>();

    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            const distances = coordinates.map(coord => manhattanDistance([x, y], coord));
            const minDistance = Math.min(...distances);
            const closest = distances.filter(d => d === minDistance).length;

            if (closest === 1) {
                const index = distances.indexOf(minDistance);
                areaCount.set(index, (areaCount.get(index) || 0) + 1);
                if (x === minX || x === maxX || y === minY || y === maxY) {
                    infiniteAreas.add(index);
                }
            }
        }
    }

    return Math.max(...Array.from(areaCount.entries())
        .filter(([index]) => !infiniteAreas.has(index))
        .map(([, count]) => count));
};

const regionSize = (coordinates: Coordinate[], threshold: number): number => {
    let count = 0;
    const minX = Math.min(...coordinates.map(c => c[0])) - threshold;
    const maxX = Math.max(...coordinates.map(c => c[0])) + threshold;
    const minY = Math.min(...coordinates.map(c => c[1])) - threshold;
    const maxY = Math.max(...coordinates.map(c => c[1])) + threshold;

    for (let x = minX; x <= maxX; x++) {
        for (let y = minY; y <= maxY; y++) {
            const totalDistance = coordinates.reduce((sum, coord) => sum + manhattanDistance([x, y], coord), 0);
            if (totalDistance < threshold) {
                count++;
            }
        }
    }
    return count;
};

const main = () => {
    const coordinates = readCoordinates('input.txt');
    
    const largestArea = largestFiniteArea(coordinates);
    console.log(`Largest finite area: ${largestArea}`);

    const safeRegionSize = regionSize(coordinates, 10000);
    console.log(`Size of the safe region: ${safeRegionSize}`);
};

main();