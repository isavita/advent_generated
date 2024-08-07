import * as fs from 'fs';

type Coordinate = [number, number];

function readInput(filePath: string): { dots: Coordinate[], folds: string[] } {
    const data = fs.readFileSync(filePath, 'utf-8').trim().split('\n');
    const dots: Coordinate[] = [];
    const folds: string[] = [];
    
    for (const line of data) {
        if (line.startsWith('fold along')) {
            folds.push(line.split(' ')[2]);
        } else if (line) {
            const [x, y] = line.split(',').map(Number);
            dots.push([x, y] as Coordinate);
        }
    }
    
    return { dots, folds };
}

function foldDots(dots: Coordinate[], fold: string): Coordinate[] {
    const [axis, value] = fold.split('=');
    const foldValue = Number(value);
    
    const foldedDots = dots.map(([x, y]) => {
        if (axis === 'y' && y > foldValue) {
            return [x, foldValue - (y - foldValue)] as Coordinate;
        }
        if (axis === 'x' && x > foldValue) {
            return [foldValue - (x - foldValue), y] as Coordinate;
        }
        return [x, y] as Coordinate;
    });

    return foldedDots.filter((dot, index, self) =>
        index === self.findIndex(([x, y]) => x === dot[0] && y === dot[1])
    );
}

function countVisibleDotsAfterFirstFold(filePath: string): number {
    const { dots, folds } = readInput(filePath);
    const firstFold = folds[0];
    const foldedDots = foldDots(dots, firstFold);
    return foldedDots.length;
}

const result = countVisibleDotsAfterFirstFold('input.txt');
console.log(result);