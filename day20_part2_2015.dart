import 'dart:io';

void main() {
  int target = int.parse(File('input.txt').readAsStringSync());

  int part1 = findHouse(target, 10, 1, 10);
  int part2 = findHouse(target, 11, 50, 11);

  print('Part 1: $part1');
  print('Part 2: $part2');
}

int findHouse(int target, int multiplier, int maxVisits, int increment) {
  List<int> houses = List.filled(target ~/ multiplier, 0);

  for (int elf = 1; elf < houses.length; elf++) {
    int visits = 0;
    for (int house = elf; house < houses.length; house += elf) {
      houses[house] += elf * increment;
      visits++;
      if (visits == maxVisits) break;
    }
  }

  for (int i = 0; i < houses.length; i++) {
    if (houses[i] >= target) return i;
  }

  return -1;
}