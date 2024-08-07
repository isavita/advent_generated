import * as fs from 'fs';
import * as readline from 'readline';

interface HappinessChange {
    person: string;
    neighbor: string;
    change: number;
}

const parseInput = async (filePath: string): Promise<HappinessChange[]> => {
    const fileStream = fs.createReadStream(filePath);
    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const happinessChanges: HappinessChange[] = [];

    for await (const line of rl) {
        const match = line.match(/(\w+) would (gain|lose) (\d+) happiness units by sitting next to (\w+)\./);
        if (match) {
            const change = parseInt(match[3], 10) * (match[2] === 'gain' ? 1 : -1);
            happinessChanges.push({ person: match[1], neighbor: match[4], change });
        }
    }

    return happinessChanges;
};

const calculateTotalHappiness = (arrangement: string[], happinessMap: Map<string, Map<string, number>>): number => {
    let totalHappiness = 0;
    for (let i = 0; i < arrangement.length; i++) {
        const person = arrangement[i];
        const leftNeighbor = arrangement[(i - 1 + arrangement.length) % arrangement.length];
        const rightNeighbor = arrangement[(i + 1) % arrangement.length];

        totalHappiness += happinessMap.get(person)!.get(leftNeighbor)!;
        totalHappiness += happinessMap.get(person)!.get(rightNeighbor)!;
    }
    return totalHappiness;
};

const findOptimalSeating = (happinessChanges: HappinessChange[]): number => {
    const happinessMap = new Map<string, Map<string, number>>();

    for (const { person, neighbor, change } of happinessChanges) {
        if (!happinessMap.has(person)) {
            happinessMap.set(person, new Map<string, number>());
        }
        happinessMap.get(person)!.set(neighbor, change);
    }

    const guests = Array.from(happinessMap.keys());
    let maxHappiness = -Infinity;

    const permute = (arr: string[], m: number = arr.length): void => {
        if (m === 1) {
            maxHappiness = Math.max(maxHappiness, calculateTotalHappiness(arr, happinessMap));
        } else {
            for (let i = 0; i < m; i++) {
                permute(arr, m - 1);
                const j = (m % 2) ? i : 0;
                [arr[m - 1], arr[j]] = [arr[j], arr[m - 1]];
            }
        }
    };

    permute(guests);
    return maxHappiness;
};

const main = async () => {
    try {
        const happinessChanges = await parseInput('input.txt');
        const maxHappiness = findOptimalSeating(happinessChanges);
        console.log(maxHappiness);
    } catch (error) {
        console.error('Error reading the input file:', error);
    }
};

main();