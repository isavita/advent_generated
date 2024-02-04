import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<String> parts = lines[0].split(' ');
  int players = int.parse(parts[0]);
  int lastMarble = int.parse(parts[6]);

  List<int> scores = List.filled(players, 0);
  List<int> circle = [0];
  int currentPlayer = 0;
  int currentMarbleIndex = 0;

  for (int marble = 1; marble <= lastMarble; marble++) {
    if (marble % 23 == 0) {
      currentMarbleIndex = (currentMarbleIndex - 7) % circle.length;
      scores[currentPlayer] += marble + circle.removeAt(currentMarbleIndex);
    } else {
      currentMarbleIndex = (currentMarbleIndex + 2) % circle.length;
      if (currentMarbleIndex == 0) {
        circle.add(marble);
        currentMarbleIndex = circle.length - 1;
      } else {
        circle.insert(currentMarbleIndex, marble);
      }
    }

    currentPlayer = (currentPlayer + 1) % players;
  }

  int maxScore = scores.reduce((value, element) => value > element ? value : element);
  print(maxScore);
}