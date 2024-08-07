import * as fs from 'fs';
import * as readline from 'readline';

interface Disc {
    positions: number;
    startPosition: number;
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const discs: Disc[] = [];

    for await (const line of rl) {
        const match = line.match(/Disc #\d+ has (\d+) positions; at time=0, it is at position (\d+)/);
        if (match) {
            const positions = parseInt(match[1], 10);
            const startPosition = parseInt(match[2], 10);
            discs.push({ positions, startPosition });
        }
    }

    let time = 0;
    while (true) {
        let capsuleFallsThrough = true;
        for (let i = 0; i < discs.length; i++) {
            const disc = discs[i];
            const currentPosition = (disc.startPosition + time + i + 1) % disc.positions;
            if (currentPosition !== 0) {
                capsuleFallsThrough = false;
                break;
            }
        }
        if (capsuleFallsThrough) {
            console.log(`Part 1: The first time you can press the button is ${time}`);
            break;
        }
        time++;
    }

    // Part Two
    const additionalDisc: Disc = { positions: 11, startPosition: 0 };
    discs.push(additionalDisc);

    time = 0;
    while (true) {
        let capsuleFallsThrough = true;
        for (let i = 0; i < discs.length; i++) {
            const disc = discs[i];
            const currentPosition = (disc.startPosition + time + i + 1) % disc.positions;
            if (currentPosition !== 0) {
                capsuleFallsThrough = false;
                break;
            }
        }
        if (capsuleFallsThrough) {
            console.log(`Part 2: The first time you can press the button is ${time}`);
            break;
        }
        time++;
    }
}

main().catch(err => console.error(err));