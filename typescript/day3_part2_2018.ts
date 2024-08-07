import * as fs from 'fs';

interface Claim {
    id: number;
    left: number;
    top: number;
    width: number;
    height: number;
}

function parseClaim(line: string): Claim {
    const regex = /#(\d+) @ (\d+),(\d+): (\d+)x(\d+)/;
    const match = line.match(regex)!;
    return {
        id: parseInt(match[1]),
        left: parseInt(match[2]),
        top: parseInt(match[3]),
        width: parseInt(match[4]),
        height: parseInt(match[5]),
    };
}

function main() {
    const claims = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(parseClaim);
    const fabric: number[][] = Array.from({ length: 1000 }, () => Array(1000).fill(0));
    const claimMap: Map<number, boolean> = new Map(claims.map(claim => [claim.id, true]));

    claims.forEach(({ left, top, width, height, id }) => {
        for (let x = left; x < left + width; x++) {
            for (let y = top; y < top + height; y++) {
                if (fabric[y][x] > 0) {
                    claimMap.set(fabric[y][x], false);
                    claimMap.set(id, false);
                }
                fabric[y][x] = id;
            }
        }
    });

    const overlappingArea = fabric.flat().filter(x => x > 0).length;
    const nonOverlappingClaimId = [...claimMap.entries()].find(([_, isUnique]) => isUnique)![0];

    console.log(overlappingArea);
    console.log(nonOverlappingClaimId);
}

main();