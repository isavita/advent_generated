const fs = require('fs');

const targetSue = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
};

function auntSue(input) {
    const lines = input.split("\n");
    for (let i = 0; i < lines.length; i++) {
        const line = lines[i];
        const [sueNum, thing1, amount1, thing2, amount2, thing3, amount3] = line.match(/Sue (\d+): (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)/).slice(1);
        
        const readingsMap = {
            [thing1]: parseInt(amount1),
            [thing2]: parseInt(amount2),
            [thing3]: parseInt(amount3),
        };

        let allRulesMatched = true;
        for (const check of ["cats", "trees"]) {
            if (readingsMap[check] !== undefined) {
                if (readingsMap[check] <= targetSue[check]) {
                    allRulesMatched = false;
                }
                delete readingsMap[check];
            }
        }

        for (const check of ["pomeranians", "goldfish"]) {
            if (readingsMap[check] !== undefined) {
                if (readingsMap[check] >= targetSue[check]) {
                    allRulesMatched = false;
                }
                delete readingsMap[check];
            }
        }

        for (const [thing, amount] of Object.entries(readingsMap)) {
            if (targetSue[thing] !== amount) {
                allRulesMatched = false;
            }
        }

        if (allRulesMatched) {
            return sueNum;
        }
    }

    throw new Error("Expect return from loop");
}

fs.readFile('input.txt', 'utf8', (err, data) => {
    if (err) {
        throw err;
    }
    const res = auntSue(data);
    console.log(res);
});