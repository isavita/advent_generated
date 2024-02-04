import 'dart:io';

void main() {
  var file = File('input.txt');
  var totalScore = 0;

  file.readAsLines().then((lines) {
    for (var line in lines) {
      var opponent = line[0];
      var roundEnd = line[2];

      var yourMove = ' ';
      if (roundEnd == 'X') {
        if (opponent == 'A') {
          yourMove = 'Z';
        } else if (opponent == 'B') {
          yourMove = 'X';
        } else {
          yourMove = 'Y';
        }
      } else if (roundEnd == 'Y') {
        if (opponent == 'A') {
          yourMove = 'X';
        } else if (opponent == 'B') {
          yourMove = 'Y';
        } else {
          yourMove = 'Z';
        }
      } else {
        if (opponent == 'A') {
          yourMove = 'Y';
        } else if (opponent == 'B') {
          yourMove = 'Z';
        } else {
          yourMove = 'X';
        }
      }

      var score = 0;
      if (yourMove == 'X') {
        score = 1;
      } else if (yourMove == 'Y') {
        score = 2;
      } else if (yourMove == 'Z') {
        score = 3;
      }

      if ((opponent == 'A' && yourMove == 'Y') ||
          (opponent == 'B' && yourMove == 'Z') ||
          (opponent == 'C' && yourMove == 'X')) {
        score += 6;
      } else if ((opponent == 'A' && yourMove == 'X') ||
          (opponent == 'B' && yourMove == 'Y') ||
          (opponent == 'C' && yourMove == 'Z')) {
        score += 3;
      }

      totalScore += score;
    }

    print(totalScore);
  }).catchError((e) => print("Error reading file: $e"));
}