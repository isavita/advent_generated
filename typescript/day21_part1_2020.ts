import * as fs from 'fs';

type Food = {
  ingredients: Set<string>;
  allergens: Set<string>;
};

const parseInput = (data: string): Food[] => {
  return data.trim().split('\n').map(line => {
    const [ingredientsPart, allergensPart] = line.split(' (contains ');
    const ingredients = new Set(ingredientsPart.split(' '));
    const allergens = new Set(allergensPart.slice(0, -1).split(', '));
    return { ingredients, allergens };
  });
};

const findSafeIngredientsCount = (foods: Food[]): number => {
  const allergenMap = new Map<string, Set<string>>();
  const allIngredients = new Set<string>();

  foods.forEach(({ ingredients, allergens }) => {
    ingredients.forEach(ingredient => allIngredients.add(ingredient));
    allergens.forEach(allergen => {
      if (!allergenMap.has(allergen)) {
        allergenMap.set(allergen, new Set(ingredients));
      } else {
        const existingIngredients = allergenMap.get(allergen)!;
        for (const ing of existingIngredients) {
          if (!ingredients.has(ing)) {
            existingIngredients.delete(ing);
          }
        }
      }
    });
  });

  const unsafeIngredients = new Set<string>();
  allergenMap.forEach(ingredients => {
    ingredients.forEach(ingredient => unsafeIngredients.add(ingredient));
  });

  let safeCount = 0;
  allIngredients.forEach(ingredient => {
    if (!unsafeIngredients.has(ingredient)) {
      safeCount += foods.reduce((count, { ingredients }) => count + (ingredients.has(ingredient) ? 1 : 0), 0);
    }
  });

  return safeCount;
};

const main = async () => {
  const data = fs.readFileSync('input.txt', 'utf-8');
  const foods = parseInput(data);
  const result = findSafeIngredientsCount(foods);
  console.log(result);
};

main().catch(console.error);