import * as fs from 'fs';

function findLowestHouseNumber(targetPresents: number): number {
    let houses: number[] = new Array(targetPresents).fill(0);

    for (let elf = 1; elf <= targetPresents / 10; elf++) {
        for (let house = elf; house < targetPresents; house += elf) {
            houses[house] += elf * 10;
        }
    }

    for (let house = 1; house < targetPresents; house++) {
        if (houses[house] >= targetPresents) {
            return house;
        }
    }

    return -1; // This should never be reached
}

function main() {
    const inputFile = 'input.txt';

    fs.readFile(inputFile, 'utf8', (err, data) => {
        if (err) {
            console.error(`Error reading file: ${err}`);
            return;
        }

        const targetPresents = parseInt(data.trim(), 10);
        if (isNaN(targetPresents)) {
            console.error('Invalid input. Please provide a valid number.');
            return;
        }

        const lowestHouseNumber = findLowestHouseNumber(targetPresents);
        console.log(`The lowest house number is: ${lowestHouseNumber}`);
    });
}

main();