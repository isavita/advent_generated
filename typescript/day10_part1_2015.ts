import * as fs from 'fs';
import * as readline from 'readline';

// Function to generate the next look-and-say sequence
function lookAndSay(sequence: string): string {
    let result = '';
    let count = 1;

    for (let i = 1; i <= sequence.length; i++) {
        if (i === sequence.length || sequence[i] !== sequence[i - 1]) {
            result += count + sequence[i - 1];
            count = 1;
        } else {
            count++;
        }
    }

    return result;
}

// Function to read input from file and process the look-and-say sequence
async function processInput(filePath: string): Promise<void> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    for await (const line of rl) {
        let sequence = line.trim();
        for (let i = 0; i < 40; i++) {
            sequence = lookAndSay(sequence);
        }
        console.log(sequence.length);
    }
}

// Main function to start the process
(async () => {
    try {
        await processInput('input.txt');
    } catch (error) {
        console.error('Error reading file:', error);
    }
})();