import * as fs from 'fs';
import * as readline from 'readline';

// Function to check if a passphrase is valid
function isValidPassphrase(passphrase: string): boolean {
    const words = passphrase.split(' ');
    const wordSet = new Set(words);
    return words.length === wordSet.size;
}

// Function to read input file and count valid passphrases
async function countValidPassphrases(filePath: string): Promise<number> {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let validPassphrasesCount = 0;

    for await (const line of rl) {
        if (isValidPassphrase(line)) {
            validPassphrasesCount++;
        }
    }

    return validPassphrasesCount;
}

// Main function to execute the program
(async () => {
    const filePath = 'input.txt';
    try {
        const validPassphrasesCount = await countValidPassphrases(filePath);
        console.log(`Number of valid passphrases: ${validPassphrasesCount}`);
    } catch (error) {
        // Cast error to the Error type to access the message property
        console.error(`Error reading file: ${(error as Error).message}`);
    }
})();