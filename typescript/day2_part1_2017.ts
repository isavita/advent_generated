import * as fs from 'fs';
import * as readline from 'readline';

async function processLineByLine() {
    const fileStream = fs.createReadStream('input.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    let checksum = 0;

    for await (const line of rl) {
        const numbers = line.split(/\s+/).map(Number);
        const min = Math.min(...numbers);
        const max = Math.max(...numbers);
        checksum += max - min;
    }

    console.log(checksum);
}

processLineByLine().catch(console.error);