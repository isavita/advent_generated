import * as fs from 'fs';

// Function to determine the final floor based on the instructions
function determineFloor(instructions: string): number {
    let floor = 0;

    for (const char of instructions) {
        if (char === '(') {
            floor++;
        } else if (char === ')') {
            floor--;
        }
    }

    return floor;
}

// Read the input from the file
const filePath = 'input.txt';
fs.readFile(filePath, 'utf8', (err, data) => {
    if (err) {
        console.error('Error reading the file:', err);
        return;
    }

    // Process the input and determine the final floor
    const finalFloor = determineFloor(data.trim());

    // Print the result to the standard output
    console.log(`The final floor is: ${finalFloor}`);
});