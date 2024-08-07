import * as fs from 'fs';
import * as readline from 'readline';

// Function to check if a triangle is valid
function isValidTriangle(sides: number[]): boolean {
    const [a, b, c] = sides.sort((x, y) => x - y);
    return a + b > c;
}

// Function to read the input file and process the triangles
async function countValidTriangles(filePath: string, byColumns: boolean = false): Promise<number> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let triangles: number[][] = [];
    let currentTriangle: number[][] = [];

    for await (const line of rl) {
        const sides = line.trim().split(/\s+/).map(Number);
        if (byColumns) {
            for (let i = 0; i < sides.length; i++) {
                if (currentTriangle[i] === undefined) {
                    currentTriangle[i] = [];
                }
                currentTriangle[i].push(sides[i]);
                if (currentTriangle[i].length === 3) {
                    triangles.push(currentTriangle[i]);
                    currentTriangle[i] = [];
                }
            }
        } else {
            triangles.push(sides);
        }
    }

    let validTrianglesCount = 0;
    for (const triangle of triangles) {
        if (isValidTriangle(triangle)) {
            validTrianglesCount++;
        }
    }

    return validTrianglesCount;
}

// Main function to execute the program
async function main() {
    const filePath = 'input.txt';

    // Part One: Count valid triangles by rows
    const validTrianglesCountPartOne = await countValidTriangles(filePath);
    console.log(`Part One: ${validTrianglesCountPartOne}`);

    // Part Two: Count valid triangles by columns
    const validTrianglesCountPartTwo = await countValidTriangles(filePath, true);
    console.log(`Part Two: ${validTrianglesCountPartTwo}`);
}

main().catch(console.error);