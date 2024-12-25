
import * as fs from 'fs';

interface Chemical {
    name: string;
    amount: number;
}

function parseChemical(s: string): Chemical {
    const [amountStr, name] = s.split(" ");
    return { name, amount: parseInt(amountStr) };
}

function calculateOre(chem: string, amount: number, reactions: Map<string, Chemical>, ingredients: Map<string, Chemical[]>, surplus: Map<string, number>): number {
    if (chem === "ORE") {
        return amount;
    }

    const currentSurplus = surplus.get(chem) || 0;
    if (currentSurplus >= amount) {
        surplus.set(chem, currentSurplus - amount);
        return 0;
    }

    amount -= currentSurplus;
    surplus.set(chem, 0);

    const reaction = reactions.get(chem)!;
    const times = Math.ceil(amount / reaction.amount);
    let ore = 0;

    for (const ingredient of ingredients.get(chem)!) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
    }

    surplus.set(chem, (surplus.get(chem) || 0) + times * reaction.amount - amount);
    return ore;
}

function main() {
    const fileContent = fs.readFileSync("input.txt", "utf-8");
    const lines = fileContent.trim().split("\n");

    const reactions = new Map<string, Chemical>();
    const ingredients = new Map<string, Chemical[]>();

    for (const line of lines) {
        const [inputStr, outputStr] = line.split(" => ");
        const output = parseChemical(outputStr);
        const inputs = inputStr.split(", ").map(parseChemical);
        reactions.set(output.name, output);
        ingredients.set(output.name, inputs);
    }

    console.log(calculateOre("FUEL", 1, reactions, ingredients, new Map<string, number>()));
}

main();
