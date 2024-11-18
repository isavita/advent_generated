
import 'dart:io';
import 'dart:math';

void main() {
  String input = File('input.txt').readAsStringSync().trim();
  print(solve(input));
}

Map<String, List<String>> parseInput(String input) {
  List<String> blocks = input.split('\n\n');
  List<String> startingMaterial = splitMolecules(blocks[1]);

  Map<String, List<String>> graph = {};

  for (String l in blocks[0].split('\n')) {
    List<String> parts = l.split(' => ');
    graph.putIfAbsent(parts[0], () => []).add(parts[1]);
  }

  return graph;
}

List<String> splitMolecules(String input) {
  List<String> molecules = [];
  for (int i = 0; i < input.length; i++) {
    int code = input.codeUnitAt(i);
    if (code >= 'A'.codeUnitAt(0) && code <= 'Z'.codeUnitAt(0)) {
      molecules.add(input[i]);
    } else {
      molecules[molecules.length - 1] += input[i];
    }
  }
  return molecules;
}

int solve(String input) {
  Map<String, List<String>> reverseGraph = parseInput(input);

  Map<String, String> productToReactant = {};
  reverseGraph.forEach((react, products) {
    for (String p in products) {
      if (productToReactant.containsKey(p)) {
        throw 'dup found';
      }
      productToReactant[p] = react;
    }
  });

  List<String> allProducts = productToReactant.keys.toList();
  
  String start = splitMolecules(input.split('\n\n')[1]).join();
  String mol = start;

  int steps = 0;
  final random = Random();

  while (mol != 'e') {
    bool changeMade = false;
    for (String prod in allProducts) {
      int count = mol.split(prod).length - 1;
      if (count <= 0) continue;

      changeMade = true;
      steps += count;
      mol = mol.replaceAll(prod, productToReactant[prod]!);

      break;
    }

    if (!changeMade) {
      allProducts.shuffle(random);
      mol = start;
      steps = 0;
    }
  }

  return steps;
}
