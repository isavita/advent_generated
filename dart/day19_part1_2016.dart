import 'dart:io';

void main() {
  final totalElves = readInput("input.txt");
  final winner = findWinningElf(totalElves);
  print(winner);
}

int readInput(String filename) {
  final file = File(filename);
  final totalElves = int.parse(file.readAsLinesSync().first);
  return totalElves;
}

int findWinningElf(int totalElves) {
  var highestPowerOfTwo = 1;
  while (highestPowerOfTwo * 2 <= totalElves) {
    highestPowerOfTwo *= 2;
  }
  return (totalElves - highestPowerOfTwo) * 2 + 1;
}