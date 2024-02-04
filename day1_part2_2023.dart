import 'dart:io';

void main() {
  var file = new File('input.txt');
  var sum = 0;

  file.readAsLines().then((List<String> lines) {
    lines.forEach((line) {
      var digits = ['zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'];
      var firstDigit = 0;
      var lastDigit = 0;

      for (var i = 0; i < line.length; i++) {
        var char = line[i];
        var digitStr = char;

        if (digitStr.contains(RegExp(r'[0-9]'))) {
          if (firstDigit == 0) {
            firstDigit = int.parse(digitStr);
          }
          lastDigit = int.parse(digitStr);
        } else {
          for (var j = 0; j < digits.length; j++) {
            if (line.substring(i).startsWith(digits[j])) {
              if (firstDigit == 0) {
                firstDigit = j;
              }
              lastDigit = j;
              break;
            }
          }
        }
      }

      sum += 10 * firstDigit + lastDigit;
    });

    print(sum);
  });
}