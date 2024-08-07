import * as fs from 'fs';

interface Food {
    ingredients: Set<string>;
    allergens: Set<string>;
}

const parseInput = (data: string): Food[] => {
    return data.trim().split('\n').map(line => {
        const [ingredientsPart, allergensPart] = line.split(' (contains ');
        const ingredients = new Set(ingredientsPart.split(' '));
        const allergens = new Set(allergensPart.replace(')', '').split(', '));
        return { ingredients, allergens };
    });
};

const findSafeIngredients = (foods: Food[]): { safeIngredients: Set<string>, allergenMap: Map<string, string>, ingredientCount: Map<string, number> } => {
    const allergenCandidates = new Map<string, Set<string>>();
    const ingredientCount = new Map<string, number>();

    foods.forEach(food => {
        food.ingredients.forEach(ingredient => {
            ingredientCount.set(ingredient, (ingredientCount.get(ingredient) || 0) + 1);
        });
        food.allergens.forEach(allergen => {
            if (!allergenCandidates.has(allergen)) {
                allergenCandidates.set(allergen, new Set(food.ingredients));
            } else {
                const currentCandidates = allergenCandidates.get(allergen)!;
                for (const ingredient of currentCandidates) {
                    if (!food.ingredients.has(ingredient)) {
                        currentCandidates.delete(ingredient);
                    }
                }
            }
        });
    });

    const allAllergens = Array.from(allergenCandidates.keys());
    const allergenMap = new Map<string, string>();
    const resolvedAllergens = new Set<string>();

    while (allAllergens.length) {
        for (const allergen of allAllergens) {
            const candidates = allergenCandidates.get(allergen)!;
            if (candidates.size === 1) {
                const ingredient = Array.from(candidates)[0];
                allergenMap.set(allergen, ingredient);
                resolvedAllergens.add(ingredient);
                allergenCandidates.delete(allergen);
                allAllergens.splice(allAllergens.indexOf(allergen), 1);
                allergenCandidates.forEach(c => c.delete(ingredient));
                break;
            }
        }
    }

    const safeIngredients = new Set<string>(
        [...ingredientCount.keys()].filter(ingredient => !resolvedAllergens.has(ingredient))
    );

    return { safeIngredients, allergenMap, ingredientCount };
};

const main = () => {
    const input = fs.readFileSync('input.txt', 'utf-8');
    const foods = parseInput(input);
    const { safeIngredients, allergenMap, ingredientCount } = findSafeIngredients(foods);

    const safeIngredientCount = [...safeIngredients].reduce((sum, ingredient) => sum + (ingredientCount.get(ingredient) || 0), 0);
    console.log(safeIngredientCount);

    const canonicalDangerousList = [...allergenMap.entries()]
        .sort(([a], [b]) => a.localeCompare(b))
        .map(([, ingredient]) => ingredient)
        .join(',');

    console.log(canonicalDangerousList);
};

main();