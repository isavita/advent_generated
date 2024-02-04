import 'dart:io';

void main() {
  var sum = 0;
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  for (var line in lines) {
    if (line.isEmpty) {
      continue;
    }
    var firstDigit = -1;
    var lastDigit = -1;

    for (var i = 0; i < line.length; i++) {
      if (line[i].contains(RegExp(r'[0-9]'))) {
        if (firstDigit == -1) {
          firstDigit = int.parse(line[i]);
        }
        lastDigit = int.parse(line[i]);
      }
    }

    if (firstDigit != -1 && lastDigit != -1) {
      var value = int.parse('$firstDigit$lastDigit');
      sum += value;
    }
  }

  print(sum);
}