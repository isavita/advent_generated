import * as fs from 'fs';

type Cube = { x: number; y: number; z: number; active: boolean };

const getNeighbors = (cube: Cube): Cube[] => {
    const neighbors: Cube[] = [];
    for (let dx = -1; dx <= 1; dx++) {
        for (let dy = -1; dy <= 1; dy++) {
            for (let dz = -1; dz <= 1; dz++) {
                if (dx === 0 && dy === 0 && dz === 0) continue;
                neighbors.push({ x: cube.x + dx, y: cube.y + dy, z: cube.z + dz, active: false });
            }
        }
    }
    return neighbors;
};

const simulateCycles = (initialState: Set<string>, cycles: number): Set<string> => {
    let currentState = new Set(initialState);

    for (let cycle = 0; cycle < cycles; cycle++) {
        const nextState = new Set<string>();
        const potentialCubes = new Set<string>(currentState);

        currentState.forEach(cube => {
            const [x, y, z] = cube.split(',').map(Number);
            getNeighbors({ x, y, z, active: true }).forEach(neighbor => {
                potentialCubes.add(`${neighbor.x},${neighbor.y},${neighbor.z}`);
            });
        });

        potentialCubes.forEach(cube => {
            const [x, y, z] = cube.split(',').map(Number);
            const isActive = currentState.has(cube);
            const activeNeighbors = getNeighbors({ x, y, z, active: false }).filter(neighbor => currentState.has(`${neighbor.x},${neighbor.y},${neighbor.z}`)).length;

            if (isActive && (activeNeighbors === 2 || activeNeighbors === 3)) {
                nextState.add(cube);
            } else if (!isActive && activeNeighbors === 3) {
                nextState.add(cube);
            }
        });

        currentState = nextState;
    }

    return currentState;
};

const readInput = (filePath: string): Set<string> => {
    const input = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    const activeCubes = new Set<string>();
    input.forEach((line, y) => {
        [...line].forEach((char, x) => {
            if (char === '#') {
                activeCubes.add(`${x},${y},0`);
            }
        });
    });
    return activeCubes;
};

const main = () => {
    const initialState = readInput('input.txt');
    const activeCubesAfterCycles = simulateCycles(initialState, 6);
    console.log(activeCubesAfterCycles.size);
};

main();