import 'dart:io';

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();

  var totalScore = 0;

  for (var line in lines) {
    var opponent = line[0];
    var yourMove = line[2];

    var score = 0;
    if (yourMove == 'X') {
      score = 1;
    } else if (yourMove == 'Y') {
      score = 2;
    } else if (yourMove == 'Z') {
      score = 3;
    }

    if ((opponent == 'A' && yourMove == 'Y') || (opponent == 'B' && yourMove == 'Z') || (opponent == 'C' && yourMove == 'X')) {
      score += 6;
    } else if (opponent == 'A' && yourMove == 'X' || opponent == 'B' && yourMove == 'Y' || opponent == 'C' && yourMove == 'Z') {
      score += 3;
    }

    totalScore += score;
  }

  print(totalScore);
}