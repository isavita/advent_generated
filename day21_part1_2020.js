const fs = require('fs');

const input = fs.readFileSync('input.txt', 'utf8').trim().split('\n');

const allergenMap = {};
const ingredientCount = {};
const safeIngredients = {};

for (const line of input) {
    const [ingredients, allergens] = line.split(' (contains ');
    const ingredientList = ingredients.split(' ');
    const allergenList = allergens.substring(0, allergens.length - 1).split(', ');

    for (const ingredient of ingredientList) {
        ingredientCount[ingredient] = (ingredientCount[ingredient] || 0) + 1;
        safeIngredients[ingredient] = true;
    }

    for (const allergen of allergenList) {
        if (!allergenMap[allergen]) {
            allergenMap[allergen] = {};
            for (const ingredient of ingredientList) {
                allergenMap[allergen][ingredient] = true;
            }
        } else {
            for (const ingredient in allergenMap[allergen]) {
                if (!ingredientList.includes(ingredient)) {
                    delete allergenMap[allergen][ingredient];
                }
            }
        }
    }
}

for (const ingredients of Object.values(allergenMap)) {
    for (const ingredient in ingredients) {
        delete safeIngredients[ingredient];
    }
}

let count = 0;
for (const ingredient in safeIngredients) {
    count += ingredientCount[ingredient];
}

console.log(count);