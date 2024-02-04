
import 'dart:io';

void main() {
  var target = int.parse(File('input.txt').readAsStringSync().trim());
  var houses = List.filled(target ~/ 10, 0);

  for (var elf = 1; elf < houses.length; elf++) {
    for (var house = elf; house < houses.length; house += elf) {
      houses[house] += elf * 10;
    }
  }

  for (var i = 0; i < houses.length; i++) {
    if (houses[i] >= target) {
      print(i);
      break;
    }
  }
}
