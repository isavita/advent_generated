import * as fs from 'fs';
import * as readline from 'readline';

interface Rule {
    name: string;
    ranges: [number, number][];
}

class Rule {
    constructor(public name: string, public ranges: [number, number][]) {}

    isValid(value: number): boolean {
        return this.ranges.some(rng => value >= rng[0] && value <= rng[1]);
    }
}

async function main() {
    const fileStream = fs.createReadStream('input.txt');
    const rl = readline.createInterface({ input: fileStream, crlfDelay: Infinity });

    const rules: Rule[] = [];
    const myTicket: number[] = [];
    const nearbyTickets: number[][] = [];
    let section = 0;
    const reRule = /^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/;

    for await (const line of rl) {
        if (line === '') {
            section++;
            continue;
        }
        switch (section) {
            case 0:
                const parts = reRule.exec(line);
                if (parts) {
                    rules.push(new Rule(parts[1], [
                        [parseInt(parts[2]), parseInt(parts[3])],
                        [parseInt(parts[4]), parseInt(parts[5])],
                    ]));
                }
                break;
            case 1:
                if (line !== 'your ticket:') {
                    myTicket.push(...parseTicket(line));
                }
                break;
            case 2:
                if (line !== 'nearby tickets:') {
                    const ticket = parseTicket(line);
                    if (isValidTicket(ticket, rules)) {
                        nearbyTickets.push(ticket);
                    }
                }
                break;
        }
    }

    const fieldPositions = solveFieldPositions(rules, nearbyTickets);
    const departureProduct = calculateDepartureProduct(myTicket, fieldPositions);

    console.log(departureProduct);
}

function parseTicket(s: string): number[] {
    return s.split(',').map(Number);
}

function isValidTicket(ticket: number[], rules: Rule[]): boolean {
    return ticket.every(value => isValidForAnyRule(value, rules));
}

function isValidForAnyRule(value: number, rules: Rule[]): boolean {
    return rules.some(rule => rule.isValid(value));
}

function solveFieldPositions(rules: Rule[], tickets: number[][]): Map<string, number> {
    const validPositions = new Map<string, Map<number, boolean>>();

    for (const rule of rules) {
        validPositions.set(rule.name, new Map<number, boolean>());
        for (let i = 0; i < tickets[0].length; i++) {
            let valid = true;
            for (const ticket of tickets) {
                if (!rule.isValid(ticket[i])) {
                    valid = false;
                    break;
                }
            }
            if (valid) {
                validPositions.get(rule.name)!.set(i, true);
            }
        }
    }

    const fieldPositions = new Map<string, number>();
    while (fieldPositions.size < rules.length) {
        for (const [name, positions] of validPositions) {
            if (positions.size === 1) {
                const pos = positions.keys().next().value;
                fieldPositions.set(name, pos);
                for (const otherName of validPositions.keys()) {
                    validPositions.get(otherName)!.delete(pos);
                }
                validPositions.delete(name);
            }
        }
    }
    return fieldPositions;
}

function calculateDepartureProduct(ticket: number[], fieldPositions: Map<string, number>): number {
    let product = 1;
    for (const [name, pos] of fieldPositions) {
        if (name.startsWith('departure')) {
            product *= ticket[pos];
        }
    }
    return product;
}

main().catch(console.error);