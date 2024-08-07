import * as fs from 'fs';

// Function to calculate the wrapping paper needed for a present
function calculateWrappingPaper(dimensions: number[]): number {
    const [l, w, h] = dimensions.sort((a, b) => a - b);
    const surfaceArea = 2 * (l * w + w * h + h * l);
    const smallestSideArea = l * w;
    return surfaceArea + smallestSideArea;
}

// Function to calculate the ribbon needed for a present
function calculateRibbon(dimensions: number[]): number {
    const [l, w, h] = dimensions.sort((a, b) => a - b);
    const smallestPerimeter = 2 * (l + w);
    const bowLength = l * w * h;
    return smallestPerimeter + bowLength;
}

// Read input from file
const input = fs.readFileSync('input.txt', 'utf-8');
const lines = input.trim().split('\n');

let totalWrappingPaper = 0;
let totalRibbon = 0;

// Process each line in the input file
lines.forEach(line => {
    const dimensions = line.split('x').map(Number);
    totalWrappingPaper += calculateWrappingPaper(dimensions);
    totalRibbon += calculateRibbon(dimensions);
});

// Output the results
console.log(`Total square feet of wrapping paper: ${totalWrappingPaper}`);
console.log(`Total feet of ribbon: ${totalRibbon}`);