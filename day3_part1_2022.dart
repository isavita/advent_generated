
import 'dart:io';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();

  int sum = 0;

  for (String line in lines) {
    Set<String> firstCompartment = line.substring(0, line.length ~/ 2).split('').toSet();
    Set<String> secondCompartment = line.substring(line.length ~/ 2).split('').toSet();

    Set<String> commonItems = firstCompartment.intersection(secondCompartment);

    for (String item in commonItems) {
      int priority = item.codeUnitAt(0) - (item == item.toLowerCase() ? 96 : 38);
      sum += priority;
    }
  }

  print(sum);
}
