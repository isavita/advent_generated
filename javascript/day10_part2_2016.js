const fs = require('fs');

class Bot {
    constructor() {
        this.lowTo = '';
        this.highTo = '';
        this.chips = [];
    }
}

function minMax(a, b) {
    if (a < b) {
        return [a, b];
    }
    return [b, a];
}

function giveChip(bots, outputs, target, value) {
    if (target.startsWith('bot')) {
        if (!bots[target]) {
            bots[target] = new Bot();
        }
        bots[target].chips.push(value);
    } else if (target.startsWith('output')) {
        outputs[target] = value;
    }
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        console.error(err);
        return;
    }

    const bots = {};
    const outputs = {};
    const valueRegex = /value (\d+) goes to (bot \d+)/;
    const givesRegex = /(bot \d+) gives low to (bot \d+|output \d+) and high to (bot \d+|output \d+)/;

    data.split('\n').forEach(line => {
        if (valueRegex.test(line)) {
            const matches = line.match(valueRegex);
            const value = parseInt(matches[1]);
            const botID = matches[2];

            if (!bots[botID]) {
                bots[botID] = new Bot();
            }
            bots[botID].chips.push(value);

        } else if (givesRegex.test(line)) {
            const matches = line.match(givesRegex);
            const botID = matches[1];
            const lowTo = matches[2];
            const highTo = matches[3];

            if (!bots[botID]) {
                bots[botID] = new Bot();
            }
            bots[botID].lowTo = lowTo;
            bots[botID].highTo = highTo;
        }
    });

    while (true) {
        let action = false;
        for (const botID in bots) {
            const b = bots[botID];
            if (b.chips.length === 2) {
                action = true;
                const [low, high] = minMax(b.chips[0], b.chips[1]);
                b.chips = [];

                giveChip(bots, outputs, b.lowTo, low);
                giveChip(bots, outputs, b.highTo, high);
            }
        }
        if (!action) {
            break;
        }
    }

    const result = outputs['output 0'] * outputs['output 1'] * outputs['output 2'];
    console.log(result);
});