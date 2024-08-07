import * as fs from 'fs';
import * as readline from 'readline';

async function countValidTriangles(filePath: string): Promise<void> {
    const fileStream = fs.createReadStream(filePath);

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let validTrianglesCount = 0;

    for await (const line of rl) {
        const sides = line.trim().split(/\s+/).map(Number);
        if (isTriangleValid(sides)) {
            validTrianglesCount++;
        }
    }

    console.log(validTrianglesCount);
}

function isTriangleValid(sides: number[]): boolean {
    const [a, b, c] = sides.sort((x, y) => x - y);
    return a + b > c;
}

countValidTriangles('input.txt').catch(console.error);