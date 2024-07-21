
import 'dart:io';

class Chemical {
  String name;
  int amount;

  Chemical(this.name, this.amount);
}

Chemical parseChemical(String s) {
  var parts = s.split(' ');
  return Chemical(parts[1], int.parse(parts[0]));
}

int calculateOre(String chem, int amount, Map<String, Chemical> reactions, Map<String, List<Chemical>> ingredients, Map<String, int> surplus) {
  if (chem == 'ORE') return amount;

  if (surplus[chem] != null && surplus[chem]! >= amount) {
    surplus[chem] = surplus[chem]! - amount;
    return 0;
  }

  amount -= surplus[chem] ?? 0;
  surplus[chem] = 0;
  var reaction = reactions[chem]!;
  var times = (amount + reaction.amount - 1) ~/ reaction.amount;
  var ore = 0;

  for (var ingredient in ingredients[chem]!) {
    ore += calculateOre(ingredient.name, ingredient.amount * times, reactions, ingredients, surplus);
  }

  surplus[chem] = (surplus[chem] ?? 0) + times * reaction.amount - amount;
  return ore;
}

void main() async {
  var file = File('input.txt');
  var lines = await file.readAsLines();

  var reactions = <String, Chemical>{};
  var ingredients = <String, List<Chemical>>{};

  for (var line in lines) {
    var parts = line.split(' => ');
    var output = parseChemical(parts[1]);
    var inputs = parts[0].split(', ').map(parseChemical).toList();
    reactions[output.name] = output;
    ingredients[output.name] = inputs;
  }

  print(calculateOre('FUEL', 1, reactions, ingredients, {}));
}
