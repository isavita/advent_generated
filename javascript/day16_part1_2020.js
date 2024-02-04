const fs = require('fs');

class Rule {
    constructor(name, ranges) {
        this.name = name;
        this.ranges = ranges;
    }

    isValid(value) {
        for (let i = 0; i < this.ranges.length; i++) {
            if (value >= this.ranges[i][0] && value <= this.ranges[i][1]) {
                return true;
            }
        }
        return false;
    }
}

function toInt(s) {
    return parseInt(s);
}

function isValidForAnyRule(value, rules) {
    for (let i = 0; i < rules.length; i++) {
        if (rules[i].isValid(value)) {
            return true;
        }
    }
    return false;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error("Error reading file:", err);
        return;
    }

    const lines = data.split('\n');
    const rules = [];
    let scanningRules = true;
    let errorRate = 0;

    const reRule = new RegExp('^([^:]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)$');

    lines.forEach(line => {
        if (line === "") {
            return;
        }
        if (line.startsWith("your ticket:") || line.startsWith("nearby tickets:")) {
            scanningRules = false;
            return;
        }
        if (scanningRules) {
            const matches = line.match(reRule);
            if (matches) {
                const name = matches[1];
                const range1 = [toInt(matches[2]), toInt(matches[3])];
                const range2 = [toInt(matches[4]), toInt(matches[5])];
                rules.push(new Rule(name, [range1, range2]));
            }
        } else {
            const values = line.split(",");
            values.forEach(value => {
                const val = toInt(value);
                if (!isValidForAnyRule(val, rules)) {
                    errorRate += val;
                }
            });
        }
    });

    console.log(errorRate);
});