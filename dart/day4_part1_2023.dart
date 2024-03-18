import 'dart:io';

void main() {
  final file = File('input.txt');
  final totalPoints = file.readAsLinesSync().fold(0, (points, line) {
    final parts = line.split(' | ');
    final winningNumbers = parts[0].split(' ').map(int.tryParse).whereType<int>().toList();
    final yourNumbers = parts[1].split(' ').map(int.tryParse).whereType<int>().toList();
    return points + calculatePoints(winningNumbers, yourNumbers);
  });
  print(totalPoints);
}

int calculatePoints(List<int> winningNumbers, List<int> yourNumbers) {
  var points = 0;
  for (final num in yourNumbers) {
    if (winningNumbers.contains(num)) {
      if (points == 0) {
        points = 1;
      } else {
        points *= 2;
      }
    }
  }
  return points;
}