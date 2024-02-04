
import 'dart:io';
import 'dart:convert';

void main() {
  var file = File('input.txt');
  var contents = file.readAsStringSync();

  var matches = RegExp(r'row (\d+), column (\d+)').firstMatch(contents);
  if (matches == null) {
    throw Exception('Invalid input format.');
  }

  var row = int.parse(matches.group(1)!);
  var column = int.parse(matches.group(2)!);

  var pos = getPosition(row, column);
  var code = getCode(pos);

  print(code);
}

int getPosition(int row, int column) {
  return (row + column - 2) * (row + column - 1) ~/ 2 + column;
}

int getCode(int position) {
  const startCode = 20151125;
  const multiplier = 252533;
  const modulus = 33554393;

  var code = startCode;
  for (var i = 1; i < position; i++) {
    code = (code * multiplier) % modulus;
  }
  return code;
}
