import 'dart:io';
import 'dart:collection';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final allergenMap = <String, Set<String>>{};
  final ingredientAllergen = <String, String>{};

  for (final line in lines) {
    final parts = line.split(' (contains ');
    final ingredients = parts[0].split(' ');
    final allergens = parts[1].substring(0, parts[1].length - 1).split(', ');

    for (final allergen in allergens) {
      if (!allergenMap.containsKey(allergen)) {
        allergenMap[allergen] = ingredients.toSet();
      } else {
        allergenMap[allergen]!.retainAll(ingredients);
      }
    }
  }

  while (allergenMap.isNotEmpty) {
    final uniqueAllergens = allergenMap.entries.where((e) => e.value.length == 1).toList();
    for (final entry in uniqueAllergens) {
      final ingredient = entry.value.single;
      ingredientAllergen[entry.key] = ingredient;
      _removeIngredientFromAll(allergenMap, ingredient);
      allergenMap.remove(entry.key);
    }
  }

  final result = <String>[];
  for (final allergen in ingredientAllergen.keys.toList()..sort()) {
    result.add(ingredientAllergen[allergen]!);
  }

  print(result.join(','));
}

void _removeIngredientFromAll(Map<String, Set<String>> allergenMap, String ingredient) {
  for (final ingredients in allergenMap.values) {
    ingredients.remove(ingredient);
  }
}