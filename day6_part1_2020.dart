
import 'dart:io';

void main() {
  List<String> groups = File('input.txt').readAsLinesSync();

  int sum = 0;
  String currentGroup = '';
  
  for (String line in groups) {
    if (line.isEmpty) {
      sum += currentGroup.split('').toSet().length;
      currentGroup = '';
    } else {
      currentGroup += line;
    }
  }

  sum += currentGroup.split('').toSet().length;

  print(sum);
}
