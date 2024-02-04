
import 'dart:io';

void main() {
  String polymer = File('input.txt').readAsStringSync().trim();

  String reactPolymer(String polymer) {
    List<String> stack = [];

    for (int i = 0; i < polymer.length; i++) {
      if (stack.isNotEmpty && polymer[i] != stack.last && polymer[i].toLowerCase() == stack.last.toLowerCase()) {
        stack.removeLast();
      } else {
        stack.add(polymer[i]);
      }
    }

    return stack.join();
  }

  String reactedPolymer = reactPolymer(polymer);

  int shortestLength = reactedPolymer.length;

  for (int i = 65; i <= 90; i++) {
    String unit = String.fromCharCode(i);
    String modifiedPolymer = polymer.replaceAll(unit, '').replaceAll(unit.toLowerCase(), '');
    String reactedModifiedPolymer = reactPolymer(modifiedPolymer);
    if (reactedModifiedPolymer.length < shortestLength) {
      shortestLength = reactedModifiedPolymer.length;
    }
  }

  print(shortestLength);
}
