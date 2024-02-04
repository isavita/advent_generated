const fs = require('fs');

const rules = {};
const pattern = {};

function constructPattern(index) {
    if (rules[index].includes('|')) {
        const subrules = rules[index].split(' | ');
        const parts = [];
        for (const subrule of subrules) {
            parts.push(constructSubPattern(subrule));
        }
        return `(${parts.join('|')})`;
    }
    return constructSubPattern(rules[index]);
}

function constructSubPattern(subrule) {
    if (subrule === 'a' || subrule === 'b') {
        return subrule;
    }
    const subIdxs = subrule.split(' ');
    let pattern = '';
    for (const idx of subIdxs) {
        pattern += constructPattern(Number(idx));
    }
    return pattern;
}

function countMatches(messages) {
    const re = new RegExp(`^${pattern[0]}$`);
    let count = 0;
    for (const message of messages) {
        if (re.test(message)) {
            count++;
        }
    }
    return count;
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const lines = data.trim().split('\n');
    let i = 0;
    while (lines[i] !== '') {
        const [index, rule] = lines[i].split(': ');
        rules[Number(index)] = rule.replace(/"/g, '');
        i++;
    }

    pattern[0] = constructPattern(0);

    const messages = lines.slice(i + 1);
    const count = countMatches(messages);
    console.log('The number of messages that completely match rule 0 is:', count);
});