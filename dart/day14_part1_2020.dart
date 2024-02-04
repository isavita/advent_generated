
import 'dart:io';

void main() {
  var lines = File('input.txt').readAsLinesSync();

  var memory = {};
  var mask = '';
  
  for (var line in lines) {
    if (line.startsWith('mask')) {
      mask = line.split(' = ')[1];
    } else {
      var parts = line.split(' = ');
      var address = int.parse(parts[0].substring(4, parts[0].length - 1));
      var value = int.parse(parts[1]);

      var binaryValue = value.toRadixString(2).padLeft(36, '0');
      var newValue = '';

      for (var i = 0; i < mask.length; i++) {
        if (mask[i] == 'X') {
          newValue += binaryValue[i];
        } else {
          newValue += mask[i];
        }
      }

      memory[address] = int.parse(newValue, radix: 2);
    }
  }

  var sum = memory.values.reduce((value, element) => value + element);
  print(sum);
}
