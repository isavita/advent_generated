import * as fs from 'fs';

interface Rule {
    name: string;
    ranges: [number, number][];
}

function parseInput(input: string): { rules: Rule[], myTicket: number[], nearbyTickets: number[][] } {
    const sections = input.split('\n\n');
    const rules: Rule[] = [];
    const myTicket: number[] = [];
    const nearbyTickets: number[][] = [];

    // Parse rules
    const ruleLines = sections[0].split('\n');
    for (const line of ruleLines) {
        const [name, ranges] = line.split(': ');
        const rangePairs = ranges.split(' or ').map(range => range.split('-').map(Number) as [number, number]);
        rules.push({ name, ranges: rangePairs });
    }

    // Parse my ticket
    myTicket.push(...sections[1].split('\n')[1].split(',').map(Number));

    // Parse nearby tickets
    const ticketLines = sections[2].split('\n').slice(1);
    for (const line of ticketLines) {
        nearbyTickets.push(line.split(',').map(Number));
    }

    return { rules, myTicket, nearbyTickets };
}

function isValidValue(value: number, rules: Rule[]): boolean {
    return rules.some(rule => rule.ranges.some(([min, max]) => value >= min && value <= max));
}

function calculateErrorRate(tickets: number[][], rules: Rule[]): number {
    let errorRate = 0;
    for (const ticket of tickets) {
        for (const value of ticket) {
            if (!isValidValue(value, rules)) {
                errorRate += value;
            }
        }
    }
    return errorRate;
}

function main() {
    const input = fs.readFileSync('input.txt', 'utf-8').trim();
    const { rules, nearbyTickets } = parseInput(input);
    const errorRate = calculateErrorRate(nearbyTickets, rules);
    console.log(errorRate);
}

main();