const fs = require('fs');

class Rule {
    constructor(name, ranges) {
        this.name = name;
        this.ranges = ranges;
    }

    isValid(value) {
        for (let rng of this.ranges) {
            if (value >= rng[0] && value <= rng[1]) {
                return true;
            }
        }
        return false;
    }
}

function toInt(s) {
    return parseInt(s);
}

function parseTicket(s) {
    return s.split(",").map(toInt);
}

function isValidTicket(ticket, rules) {
    for (let value of ticket) {
        if (!isValidForAnyRule(value, rules)) {
            return false;
        }
    }
    return true;
}

function isValidForAnyRule(value, rules) {
    for (let rule of rules) {
        if (rule.isValid(value)) {
            return true;
        }
    }
    return false;
}

function solveFieldPositions(rules, tickets) {
    let validPositions = {};
    for (let rule of rules) {
        validPositions[rule.name] = {};
        for (let i = 0; i < tickets[0].length; i++) {
            let valid = true;
            for (let ticket of tickets) {
                if (!rule.isValid(ticket[i])) {
                    valid = false;
                    break;
                }
            }
            if (valid) {
                validPositions[rule.name][i] = true;
            }
        }
    }

    let fieldPositions = {};
    while (Object.keys(fieldPositions).length < rules.length) {
        for (let name in validPositions) {
            if (Object.keys(validPositions[name]).length === 1) {
                let pos = Object.keys(validPositions[name])[0];
                fieldPositions[name] = pos;
                for (let otherName in validPositions) {
                    delete validPositions[otherName][pos];
                }
                delete validPositions[name];
            }
        }
    }
    return fieldPositions;
}

function calculateDepartureProduct(ticket, fieldPositions) {
    let product = 1;
    for (let name in fieldPositions) {
        if (name.startsWith("departure")) {
            product *= ticket[fieldPositions[name]];
        }
    }
    return product;
}

const input = fs.readFileSync("input.txt", "utf8").trim().split("\n");

let rules = [];
let myTicket = [];
let nearbyTickets = [];
let section = 0;
const reRule = /^([^:]+): (\d+)-(\d+) or (\d+)-(\d+)$/;

for (let line of input) {
    if (line === "") {
        section++;
        continue;
    }
    switch (section) {
        case 0:
            let parts = line.match(reRule);
            if (parts !== null) {
                let rule = new Rule(parts[1], [
                    [toInt(parts[2]), toInt(parts[3])],
                    [toInt(parts[4]), toInt(parts[5])]
                ]);
                rules.push(rule);
            }
            break;
        case 1:
            if (line !== "your ticket:") {
                myTicket = parseTicket(line);
            }
            break;
        case 2:
            if (line !== "nearby tickets:") {
                let ticket = parseTicket(line);
                if (isValidTicket(ticket, rules)) {
                    nearbyTickets.push(ticket);
                }
            }
            break;
    }
}

let fieldPositions = solveFieldPositions(rules, nearbyTickets);
let departureProduct = calculateDepartureProduct(myTicket, fieldPositions);

console.log(departureProduct);