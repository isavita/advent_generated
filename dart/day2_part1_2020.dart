
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  int validPasswords = 0;

  for (String line in lines) {
    List<String> parts = line.split(' ');
    List<String> range = parts[0].split('-');
    int min = int.parse(range[0]);
    int max = int.parse(range[1]);
    String letter = parts[1][0];
    String password = parts[2];

    int count = password.split('').where((char) => char == letter).length;

    if (count >= min && count <= max) {
      validPasswords++;
    }
  }

  print(validPasswords);
}
