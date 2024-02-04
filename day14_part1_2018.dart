
import 'dart:io';

void main() {
  List<int> recipes = [3, 7];
  int elf1 = 0;
  int elf2 = 1;

  String input = File('input.txt').readAsStringSync().trim();
  int numRecipes = int.parse(input);

  while (recipes.length < numRecipes + 10) {
    int sum = recipes[elf1] + recipes[elf2];
    if (sum >= 10) {
      recipes.add(1);
      recipes.add(sum % 10);
    } else {
      recipes.add(sum);
    }

    elf1 = (elf1 + 1 + recipes[elf1]) % recipes.length;
    elf2 = (elf2 + 1 + recipes[elf2]) % recipes.length;
  }

  String result = '';
  for (int i = numRecipes; i < numRecipes + 10; i++) {
    result += recipes[i].toString();
  }

  print(result);
}
