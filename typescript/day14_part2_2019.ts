import * as fs from 'fs';

type Chemical = {
    name: string;
    amount: number;
};

const parseChemical = (s: string): Chemical => {
    const parts = s.split(" ");
    return { name: parts[1], amount: parseInt(parts[0]) };
};

const calculateOre = (chem: string, amount: number, reactions: Map<string, Chemical>, ingredients: Map<string, Chemical[]>, surplus: Map<string, number>): number => {
    if (chem === "ORE") return amount;

    if ((surplus.get(chem) || 0) >= amount) {
        surplus.set(chem, (surplus.get(chem) || 0) - amount);
        return 0;
    }

    amount -= surplus.get(chem) || 0;
    surplus.set(chem, 0);
    const reaction = reactions.get(chem)!;
    const times = Math.ceil(amount / reaction.amount);
    let ore = 0;

    for (const ingredient of ingredients.get(chem)!) {
        ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
    }

    surplus.set(chem, (surplus.get(chem) || 0) + times * reaction.amount - amount);
    return ore;
};

const maxFuel = (reactions: Map<string, Chemical>, ingredients: Map<string, Chemical[]>, oreAvailable: number): number => {
    let low = 0, high = oreAvailable;
    while (low < high) {
        const mid = Math.ceil((low + high + 1) / 2);
        if (calculateOre("FUEL", mid, reactions, ingredients, new Map()) > oreAvailable) {
            high = mid - 1;
        } else {
            low = mid;
        }
    }
    return low;
};

const main = () => {
    const data = fs.readFileSync("input.txt", "utf-8").trim().split("\n");
    const reactions = new Map<string, Chemical>();
    const ingredients = new Map<string, Chemical[]>();

    for (const line of data) {
        const parts = line.split(" => ");
        const output = parseChemical(parts[1]);
        const inputs = parts[0].split(", ").map(parseChemical);
        reactions.set(output.name, output);
        ingredients.set(output.name, inputs);
    }

    const oreAvailable = 1000000000000;
    console.log(maxFuel(reactions, ingredients, oreAvailable));
};

main();