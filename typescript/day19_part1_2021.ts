
import * as fs from 'fs';

class Scanner {
    number: number;
    x: number;
    y: number;
    z: number;
    relativeCoords: number[][];
    absoluteCoords: number[][];
    absoluteCoordsMap: Map<string, boolean>;
    rotations: number[][][];

    constructor(number: number, coords: number[][]) {
        this.number = number;
        this.x = 0;
        this.y = 0;
        this.z = 0;
        this.relativeCoords = coords;
        this.absoluteCoords = [];
        this.absoluteCoordsMap = new Map<string, boolean>();
        this.rotations = this.fillRotations();
    }

    fillAbsoluteCoordsMap(): void {
        if (!this.absoluteCoords) {
            throw new Error(`absolute coords not set for scanner ${this.number}`);
        }
        this.absoluteCoords.forEach(ac => {
            this.absoluteCoordsMap.set(this.tupleToString(ac), true);
        });
    }

    tupleToString(tuple: number[]): string {
        return tuple.join(',');
    }


    fillRotations(): number[][][] {
        const posX = this.relativeCoords;
        const dir2: number[][] = [];
        const dir3: number[][] = [];
        const dir4: number[][] = [];
        const dir5: number[][] = [];
        const dir6: number[][] = [];

        for (const [x, y, z] of posX) {
            dir2.push([x, -y, -z]);
            dir3.push([x, -z, y]);
            dir4.push([-y, -z, x]);
            dir5.push([-x, -z, -y]);
            dir6.push([y, -z, -x]);
        }

        const sixRotations = [posX, dir2, dir3, dir4, dir5, dir6];

        const rotations: number[][][] = [];
        for (const rotation of sixRotations) {
            const r2: number[][] = [];
            const r3: number[][] = [];
            const r4: number[][] = [];
            for (const [x, y, z] of rotation) {
                r2.push([-y, x, z]);
                r3.push([-x, -y, z]);
                r4.push([y, -x, z]);
            }
            rotations.push(rotation, r2, r3, r4);
        }
        return rotations;
    }
}

function findAbsoluteCoordsForScanner(undet: Scanner, settled: Scanner[]): [Scanner, boolean] {
    for (const rotatedCoords of undet.rotations) {
        for (const setScanner of settled) {
            for (const absCoord of setScanner.absoluteCoords) {
                for (const relativeCoord of rotatedCoords) {
                    const unsettledAbsoluteCoords = makeAbsoluteCoordsList(absCoord, relativeCoord, rotatedCoords);
                    let matchingCount = 0;
                    for (const ac of unsettledAbsoluteCoords) {
                        if (setScanner.absoluteCoordsMap.has(setScanner.tupleToString(ac))) {
                            matchingCount++;
                        }
                    }

                    if (matchingCount >= 12) {
                        undet.relativeCoords = rotatedCoords;
                        undet.absoluteCoords = unsettledAbsoluteCoords;
                        undet.fillAbsoluteCoordsMap();
                        undet.x = absCoord[0] - relativeCoord[0];
                        undet.y = absCoord[1] - relativeCoord[1];
                        undet.z = absCoord[2] - relativeCoord[2];
                        return [undet, true];
                    }
                }
            }
        }
    }
    return [undet, false];
}

function makeAbsoluteCoordsList(absolute: number[], relative: number[], relativeCoords: number[][]): number[][] {
    const diff = [absolute[0] - relative[0], absolute[1] - relative[1], absolute[2] - relative[2]];
    return relativeCoords.map(c => [diff[0] + c[0], diff[1] + c[1], diff[2] + c[2]]);
}

function parseInput(inputData: string): Scanner[] {
    const scanners: Scanner[] = [];
    for (const rawScanner of inputData.split("\n\n")) {
        const lines = rawScanner.split("\n");
        const number = parseInt(lines[0].split(" ")[2]);
        const coords: number[][] = lines.slice(1).map(line => line.split(",").map(Number));
        scanners.push(new Scanner(number, coords));
    }
    return scanners;
}

function main(): void {
    const inputData = fs.readFileSync("input.txt", "utf-8").trim();
    const scanners = parseInput(inputData);

    const settled: Scanner[] = [scanners[0]];
    settled[0].absoluteCoords = settled[0].relativeCoords;
    settled[0].fillAbsoluteCoordsMap();

    const undetermined: Scanner[] = scanners.slice(1);

    while (undetermined.length > 0) {
        for (let i = 0; i < undetermined.length; i++) {
            const undet = undetermined[i];
            const [maybeUpdated, ok] = findAbsoluteCoordsForScanner(undet, settled);
            if (ok) {
                settled.push(maybeUpdated);
                undetermined.splice(i, 1);
                break;
            }
        }
    }

    const allBeacons = new Set<string>();
    for (const s of settled) {
        s.absoluteCoordsMap.forEach((_, key) => allBeacons.add(key));
    }

    console.log(allBeacons.size);
}

main();
