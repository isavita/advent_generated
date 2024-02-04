import 'dart:io';

void main() {
  var frequencies = <int>[];
  var currentFrequency = 0;
  var firstDuplicateFrequency;

  var file = File('input.txt');
  var input = file.readAsLinesSync();

  while (firstDuplicateFrequency == null) {
    for (var line in input) {
      var change = int.parse(line);
      currentFrequency += change;

      if (frequencies.contains(currentFrequency)) {
        firstDuplicateFrequency = currentFrequency;
        break;
      }

      frequencies.add(currentFrequency);
    }
  }

  print(firstDuplicateFrequency);
}