
const fs = require('fs');

function parseChemical(s) {
    const parts = s.split(' ');
    const amount = parseInt(parts[0]);
    return { name: parts[1], amount };
}

const input = fs.readFileSync('input.txt', 'utf8').split('\n');
const reactions = new Map();
const ingredients = {};

for (const line of input) {
    const parts = line.split(' => ');
    const output = parseChemical(parts[1]);
    const inputs = parts[0].split(', ').map(parseChemical);
    reactions.set(output.name, output);
    ingredients[output.name] = inputs;
}

console.log(calculateOre('FUEL', 1, reactions, ingredients, new Map()));

function calculateOre(chem, amount, reactions, ingredients, surplus) {
    if (chem === 'ORE') {
        return amount;
    }

    if (surplus.has(chem) && surplus.get(chem) >= amount) {
        surplus.set(chem, surplus.get(chem) - amount);
        return 0;
    }

    amount -= surplus.get(chem) || 0;
    surplus.set(chem, 0);
    const reaction = reactions.get(chem);
    const times = Math.ceil(amount / reaction.amount);
    let ore = 0;

    for (const ingredient of ingredients[chem]) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
    }

    surplus.set(chem, (surplus.get(chem) || 0) + times * reaction.amount - amount);
    return ore;
}
