import * as fs from 'fs';

const input = fs.readFileSync('input.txt', 'utf-8').trim().split('\n').map(line => line.split('').map(Number));

const getAdjacentPoints = (row: number, col: number, heightmap: number[][]): number[] => {
    const directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
    const adjacent: number[] = [];
    
    for (const [dx, dy] of directions) {
        const newRow = row + dx;
        const newCol = col + dy;
        if (newRow >= 0 && newRow < heightmap.length && newCol >= 0 && newCol < heightmap[0].length) {
            adjacent.push(heightmap[newRow][newCol]);
        }
    }
    return adjacent;
};

const findLowPoints = (heightmap: number[][]): number[] => {
    const lowPoints: number[] = [];
    
    for (let i = 0; i < heightmap.length; i++) {
        for (let j = 0; j < heightmap[i].length; j++) {
            const currentHeight = heightmap[i][j];
            const adjacentHeights = getAdjacentPoints(i, j, heightmap);

            if (adjacentHeights.every(height => currentHeight < height)) {
                lowPoints.push(currentHeight);
            }
        }
    }
    return lowPoints;
};

const calculateRiskLevelSum = (lowPoints: number[]): number => {
    return lowPoints.reduce((sum, height) => sum + height + 1, 0);
};

const lowPoints = findLowPoints(input);
const riskLevelSum = calculateRiskLevelSum(lowPoints);

console.log(riskLevelSum);