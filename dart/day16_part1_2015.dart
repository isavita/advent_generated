
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  Map<String, int> mfcsam = {
    'children': 3,
    'cats': 7,
    'samoyeds': 2,
    'pomeranians': 3,
    'akitas': 0,
    'vizslas': 0,
    'goldfish': 5,
    'trees': 3,
    'cars': 2,
    'perfumes': 1,
  };
  
  for (int i = 0; i < lines.length; i++) {
    Map<String, int> sue = {};
    List<String> parts = lines[i].split(' ');
    for (int j = 2; j < parts.length; j += 2) {
      sue[parts[j].substring(0, parts[j].length - 1)] = int.parse(parts[j + 1].replaceAll(',', ''));
    }
    
    bool found = true;
    sue.forEach((key, value) {
      if (mfcsam[key] != value) {
        found = false;
      }
    });
    
    if (found) {
      print(i + 1);
      break;
    }
  }
}
