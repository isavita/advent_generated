import 'dart:io';

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();

  var repeatedInput = repeatInput(input, 10000);

  var offset = int.parse(input.substring(0, 7));

  for (var phase = 0; phase < 100; phase++) {
    var sum = 0;
    for (var i = repeatedInput.length - 1; i >= offset; i--) {
      sum += repeatedInput[i];
      repeatedInput[i] = sum % 10;
    }
  }

  for (var i = offset; i < offset + 8; i++) {
    stdout.write(repeatedInput[i]);
  }
  stdout.writeln();
}

List<int> repeatInput(String input, int times) {
  var digits = List<int>.filled(input.length * times, 0);
  for (var t = 0; t < times; t++) {
    for (var i = 0; i < input.length; i++) {
      var digit = int.parse(input[i]);
      digits[t * input.length + i] = digit;
    }
  }
  return digits;
}