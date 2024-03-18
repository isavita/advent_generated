import 'dart:io';
import 'dart:collection';

void main() async {
  Map<String, Map<String, bool>> allergenMap = {};
  Map<String, int> ingredientCount = {};
  Set<String> safeIngredients = {};

  List<String> lines = await File('input.txt').readAsLines();
  for (String line in lines) {
    List<String> parts = line.split(' (contains ');
    List<String> ingredients = parts[0].split(' ');
    List<String> allergens = parts.length > 1 ? parts[1].substring(0, parts[1].length - 1).split(', ') : [];

    for (String ingredient in ingredients) {
      ingredientCount[ingredient] = (ingredientCount[ingredient] ?? 0) + 1;
      safeIngredients.add(ingredient);
    }

    for (String allergen in allergens) {
      if (!allergenMap.containsKey(allergen)) {
        allergenMap[allergen] = {for (String ingredient in ingredients) ingredient: true};
      } else {
        allergenMap[allergen]!.removeWhere((ingredient, _) => !ingredients.contains(ingredient));
      }
    }
  }

  for (String allergen in allergenMap.keys) {
    for (String ingredient in allergenMap[allergen]!.keys) {
      safeIngredients.remove(ingredient);
    }
  }

  int count = 0;
  for (String ingredient in safeIngredients) {
    count += ingredientCount[ingredient]!;
  }

  print(count);
}