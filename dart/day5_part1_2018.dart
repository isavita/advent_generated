
import 'dart:io';

void main() {
  String polymer = File('input.txt').readAsStringSync().trim();
  
  String reactedPolymer = reactPolymer(polymer);
  
  print(reactedPolymer.length);
}

String reactPolymer(String polymer) {
  List<String> units = polymer.split('');
  List<String> result = [];
  
  for (String unit in units) {
    if (result.isNotEmpty && willReact(result.last, unit)) {
      result.removeLast();
    } else {
      result.add(unit);
    }
  }
  
  return result.join('');
}

bool willReact(String unit1, String unit2) {
  return unit1 != unit2 && unit1.toLowerCase() == unit2.toLowerCase();
}
