import * as fs from 'fs';

// Function to parse the input file and return the left and right lists
function parseInput(filename: string): { leftList: number[], rightList: number[] } {
    const data = fs.readFileSync(filename, 'utf8');
    const leftList: number[] = [];
    const rightList: number[] = [];

    data.split('\n').forEach(line => {
        if (line.trim() === '') return;
        const [left, right] = line.split(/\s+/).map(Number);
        leftList.push(left);
        rightList.push(right);
    });

    return { leftList, rightList };
}

// Function to calculate the total distance
function calculateTotalDistance(leftList: number[], rightList: number[]): number {
    const sortedLeft = [...leftList].sort((a, b) => a - b);
    const sortedRight = [...rightList].sort((a, b) => a - b);

    return sortedLeft.reduce((total, left, index) => {
        const right = sortedRight[index];
        return total + Math.abs(left - right);
    }, 0);
}

// Main function
function main(): void {
    const filename = 'input.txt';
    const { leftList, rightList } = parseInput(filename);
    const totalDistance = calculateTotalDistance(leftList, rightList);
    console.log('Total distance:', totalDistance);
}

main();
