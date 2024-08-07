import * as fs from 'fs';
import * as path from 'path';

// Function to calculate the wrapping paper needed for a single present
function calculateWrappingPaper(dimensions: number[]): number {
    const [l, w, h] = dimensions;
    const side1 = l * w;
    const side2 = w * h;
    const side3 = h * l;
    const surfaceArea = 2 * side1 + 2 * side2 + 2 * side3;
    const smallestSide = Math.min(side1, side2, side3);
    return surfaceArea + smallestSide;
}

// Function to read input from file and calculate total wrapping paper needed
function calculateTotalWrappingPaper(filePath: string): number {
    const input = fs.readFileSync(filePath, 'utf-8').trim();
    const lines = input.split('\n');
    let totalWrappingPaper = 0;

    lines.forEach(line => {
        const dimensions = line.split('x').map(Number);
        totalWrappingPaper += calculateWrappingPaper(dimensions);
    });

    return totalWrappingPaper;
}

// Main function
function main() {
    const filePath = path.join(__dirname, 'input.txt');
    const totalWrappingPaper = calculateTotalWrappingPaper(filePath);
    console.log(`Total square feet of wrapping paper needed: ${totalWrappingPaper}`);
}

// Run the main function
main();