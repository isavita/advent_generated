
import 'dart:io';

class Ingredient {
  String name;
  int capacity;
  int durability;
  int flavor;
  int texture;
  int calories;

  Ingredient(this.name, this.capacity, this.durability, this.flavor, this.texture, this.calories);
}

void main() {
  List<Ingredient> ingredients = readIngredients("input.txt");
  int maxScore = findMaxScore(ingredients, 100, 500);
  print(maxScore);
}

List<Ingredient> readIngredients(String filename) {
  File file = File(filename);
  List<Ingredient> ingredients = [];

  file.readAsLinesSync().forEach((line) {
    List<String> parts = line.split(' ');
    if (parts.length < 11) {
      return;
    }

    int capacity = int.parse(parts[2].substring(0, parts[2].length - 1));
    int durability = int.parse(parts[4].substring(0, parts[4].length - 1));
    int flavor = int.parse(parts[6].substring(0, parts[6].length - 1));
    int texture = int.parse(parts[8].substring(0, parts[8].length - 1));
    int calories = int.parse(parts[10]);

    ingredients.add(Ingredient(parts[0], capacity, durability, flavor, texture, calories));
  });

  return ingredients;
}

int findMaxScore(List<Ingredient> ingredients, int totalTeaspoons, int targetCalories) {
  return calculateMaxScore(ingredients, 0, totalTeaspoons, [], targetCalories);
}

int calculateMaxScore(List<Ingredient> ingredients, int index, int remaining, List<int> teaspoons, int targetCalories) {
  if (index == ingredients.length - 1) {
    teaspoons.add(remaining);
    if (calculateCalories(ingredients, teaspoons) == targetCalories) {
      return score(ingredients, teaspoons);
    }
    return 0;
  }

  int maxScore = 0;
  for (int i = 0; i <= remaining; i++) {
    int score = calculateMaxScore(ingredients, index + 1, remaining - i, [...teaspoons, i], targetCalories);
    if (score > maxScore) {
      maxScore = score;
    }
  }
  return maxScore;
}

int score(List<Ingredient> ingredients, List<int> teaspoons) {
  int capacity = 0, durability = 0, flavor = 0, texture = 0;
  for (int i = 0; i < ingredients.length; i++) {
    capacity += ingredients[i].capacity * teaspoons[i];
    durability += ingredients[i].durability * teaspoons[i];
    flavor += ingredients[i].flavor * teaspoons[i];
    texture += ingredients[i].texture * teaspoons[i];
  }

  if (capacity < 0) capacity = 0;
  if (durability < 0) durability = 0;
  if (flavor < 0) flavor = 0;
  if (texture < 0) texture = 0;

  return capacity * durability * flavor * texture;
}

int calculateCalories(List<Ingredient> ingredients, List<int> teaspoons) {
  int calories = 0;
  for (int i = 0; i < ingredients.length; i++) {
    calories += ingredients[i].calories * teaspoons[i];
  }
  return calories;
}
