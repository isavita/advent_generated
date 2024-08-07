import * as fs from 'fs';

type CubeState = 'active' | 'inactive';

class PocketDimension {
    private cubes: Map<string, CubeState> = new Map();
    private directions: number[][];

    constructor(initialState: string[]) {
        this.directions = this.generateDirections();
        this.initializeCubes(initialState);
    }

    private generateDirections(): number[][] {
        const directions = [];
        for (let x = -1; x <= 1; x++) {
            for (let y = -1; y <= 1; y++) {
                for (let z = -1; z <= 1; z++) {
                    for (let w = -1; w <= 1; w++) {
                        if (x !== 0 || y !== 0 || z !== 0 || w !== 0) {
                            directions.push([x, y, z, w]);
                        }
                    }
                }
            }
        }
        return directions;
    }

    private initializeCubes(initialState: string[]) {
        initialState.forEach((line, y) => {
            for (let x = 0; x < line.length; x++) {
                const state = line[x] === '#' ? 'active' : 'inactive';
                this.cubes.set(`${x},${y},0,0`, state);
            }
        });
    }

    private getActiveNeighbors(x: number, y: number, z: number, w: number): number {
        return this.directions.reduce((count, [dx, dy, dz, dw]) => {
            const neighborKey = `${x + dx},${y + dy},${z + dz},${w + dw}`;
            return count + (this.cubes.get(neighborKey) === 'active' ? 1 : 0);
        }, 0);
    }

    public simulateCycles(cycles: number) {
        for (let cycle = 0; cycle < cycles; cycle++) {
            const newCubes = new Map<string, CubeState>();
            const keys = Array.from(this.cubes.keys());
            const bounds = this.calculateBounds(keys);

            for (let x = bounds.minX - 1; x <= bounds.maxX + 1; x++) {
                for (let y = bounds.minY - 1; y <= bounds.maxY + 1; y++) {
                    for (let z = bounds.minZ - 1; z <= bounds.maxZ + 1; z++) {
                        for (let w = bounds.minW - 1; w <= bounds.maxW + 1; w++) {
                            const activeNeighbors = this.getActiveNeighbors(x, y, z, w);
                            const currentKey = `${x},${y},${z},${w}`;
                            const currentState = this.cubes.get(currentKey) || 'inactive';

                            if (currentState === 'active' && (activeNeighbors === 2 || activeNeighbors === 3)) {
                                newCubes.set(currentKey, 'active');
                            } else if (currentState === 'inactive' && activeNeighbors === 3) {
                                newCubes.set(currentKey, 'active');
                            } else {
                                newCubes.set(currentKey, 'inactive');
                            }
                        }
                    }
                }
            }
            this.cubes = newCubes;
        }
    }

    private calculateBounds(keys: string[]) {
        const coordinates = keys.map(key => key.split(',').map(Number));
        const xs = coordinates.map(coord => coord[0]);
        const ys = coordinates.map(coord => coord[1]);
        const zs = coordinates.map(coord => coord[2]);
        const ws = coordinates.map(coord => coord[3]);

        return {
            minX: Math.min(...xs),
            maxX: Math.max(...xs),
            minY: Math.min(...ys),
            maxY: Math.max(...ys),
            minZ: Math.min(...zs),
            maxZ: Math.max(...zs),
            minW: Math.min(...ws),
            maxW: Math.max(...ws),
        };
    }

    public countActiveCubes(): number {
        return Array.from(this.cubes.values()).filter(state => state === 'active').length;
    }
}

const main = async () => {
    const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n');
    const pocketDimension = new PocketDimension(input);
    pocketDimension.simulateCycles(6);
    console.log(pocketDimension.countActiveCubes());
};

main();