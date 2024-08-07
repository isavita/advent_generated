import * as fs from 'fs';

const readInput = (filePath: string): Set<string> => {
    const data = fs.readFileSync(filePath, 'utf-8');
    return new Set(data.trim().split('\n'));
};

const calculateSurfaceArea = (cubes: Set<string>): number => {
    const directions = [
        [1, 0, 0], [-1, 0, 0],
        [0, 1, 0], [0, -1, 0],
        [0, 0, 1], [0, 0, -1]
    ];
    
    let surfaceArea = 0;

    for (const cube of cubes) {
        const [x, y, z] = cube.split(',').map(Number);
        let exposedSides = 6;

        for (const [dx, dy, dz] of directions) {
            const neighbor = `${x + dx},${y + dy},${z + dz}`;
            if (cubes.has(neighbor)) {
                exposedSides--;
            }
        }

        surfaceArea += exposedSides;
    }

    return surfaceArea;
};

const main = () => {
    const cubes = readInput('input.txt');
    const surfaceArea = calculateSurfaceArea(cubes);
    console.log(surfaceArea);
};

main();