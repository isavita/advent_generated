import * as fs from 'fs';

function getPriority(item: string): number {
    const code = item.charCodeAt(0);
    return code >= 97 ? code - 96 : code - 64 + 26;
}

function calculatePrioritySum(filename: string): number {
    const data = fs.readFileSync(filename, 'utf-8').trim().split('\n');
    let totalPriority = 0;

    for (const line of data) {
        const mid = line.length / 2;
        const firstCompartment = new Set(line.slice(0, mid));
        const secondCompartment = line.slice(mid);

        for (const item of secondCompartment) {
            if (firstCompartment.has(item)) {
                totalPriority += getPriority(item);
                break;
            }
        }
    }

    return totalPriority;
}

const prioritySum = calculatePrioritySum('input.txt');
console.log(prioritySum);