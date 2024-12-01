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

// Function to calculate the similarity score
function calculateSimilarityScore(leftList: number[], rightList: number[]): number {
    // Count occurrences in the right list
    const rightFrequency: Record<number, number> = {};
    for (const num of rightList) {
        rightFrequency[num] = (rightFrequency[num] || 0) + 1;
    }

    // Compute the similarity score
    return leftList.reduce((score, num) => {
        return score + (num * (rightFrequency[num] || 0));
    }, 0);
}

// Main function
function main(): void {
    const filename = 'input.txt';
    const { leftList, rightList } = parseInput(filename);
    const similarityScore = calculateSimilarityScore(leftList, rightList);
    console.log('Similarity score:', similarityScore);
}

main();
