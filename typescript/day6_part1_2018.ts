import * as fs from 'fs';

type Point = [number, number];

const readInput = (filePath: string): Point[] => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return data.trim().split('\n').map(line => line.split(', ').map(Number) as Point);
};

const manhattanDistance = (p1: Point, p2: Point): number => Math.abs(p1[0] - p2[0]) + Math.abs(p1[1] - p2[1]);

const calculateAreas = (points: Point[], gridSize: number): number[] => {
    const areaCount = Array(points.length).fill(0);
    const infiniteAreas = new Set<number>();

    for (let x = 0; x < gridSize; x++) {
        for (let y = 0; y < gridSize; y++) {
            const distances = points.map(point => manhattanDistance(point, [x, y]));
            const minDistance = Math.min(...distances);
            const closestIndices = distances.map((d, i) => (d === minDistance ? i : -1)).filter(i => i !== -1);

            if (closestIndices.length === 1) {
                const index = closestIndices[0];
                areaCount[index]++;
                
                if (x === 0 || x === gridSize - 1 || y === 0 || y === gridSize - 1) {
                    infiniteAreas.add(index);
                }
            }
        }
    }

    return areaCount.map((count, index) => infiniteAreas.has(index) ? 0 : count);
};

const main = () => {
    const points = readInput('input.txt');
    const gridSize = 400; // Assuming a sufficiently large grid
    const areas = calculateAreas(points, gridSize);
    const largestFiniteArea = Math.max(...areas);
    console.log(largestFiniteArea);
};

main();