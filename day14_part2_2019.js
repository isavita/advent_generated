const fs = require('fs');

const reactions = fs.readFileSync('input.txt', 'utf8').trim().split('\n').map(line => {
    const [inputs, output] = line.split(' => ');
    const [quantity, chemical] = output.split(' ');
    const inputsList = inputs.split(', ').map(input => {
        const [inputQuantity, inputChemical] = input.split(' ');
        return { inputQuantity: parseInt(inputQuantity), inputChemical };
    });
    return { inputs: inputsList, output: { outputQuantity: parseInt(quantity), outputChemical: chemical } };
});

function calculateOreNeeded(fuelAmount) {
    const stock = {};
    let oreNeeded = 0;

    function produce(chemical, quantity) {
        if (chemical === 'ORE') {
            oreNeeded += quantity;
            return;
        }

        if (!stock[chemical]) {
            stock[chemical] = 0;
        }

        if (stock[chemical] >= quantity) {
            stock[chemical] -= quantity;
            return;
        }

        const reaction = reactions.find(reaction => reaction.output.outputChemical === chemical);
        const reactionTimes = Math.ceil((quantity - stock[chemical]) / reaction.output.outputQuantity);

        reaction.inputs.forEach(({ inputQuantity, inputChemical }) => {
            produce(inputChemical, inputQuantity * reactionTimes);
        });

        stock[chemical] += reaction.output.outputQuantity * reactionTimes - quantity;
    }

    produce('FUEL', fuelAmount);
    return oreNeeded;
}

const oreForOneFuel = calculateOreNeeded(1);
console.log(oreForOneFuel);

const trillion = 1000000000000;
let lowerBound = Math.floor(trillion / oreForOneFuel);
let upperBound = lowerBound * 2;

while (lowerBound < upperBound - 1) {
    const middle = Math.floor((lowerBound + upperBound) / 2);
    if (calculateOreNeeded(middle) > trillion) {
        upperBound = middle;
    } else {
        lowerBound = middle;
    }
}

console.log(lowerBound);