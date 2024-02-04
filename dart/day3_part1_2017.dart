import 'dart:io';
import 'dart:math';

void main() {
  var file = new File('input.txt');
  var input = file.readAsStringSync().trim();
  int target = int.parse(input);

  int sideLength = (sqrt(target).ceil()).toInt();
  if (sideLength % 2 == 0) {
    sideLength++;
  }

  int maxValue = sideLength * sideLength;
  int stepsFromEdge = (sideLength - 1) ~/ 2;
  int distanceToMiddle = 0;

  for (int i = 0; i < 4; i++) {
    int middlePoint = maxValue - stepsFromEdge - (sideLength - 1) * i;
    int distance = (target - middlePoint).abs();
    if (distance < distanceToMiddle || i == 0) {
      distanceToMiddle = distance;
    }
  }

  int manhattanDistance = stepsFromEdge + distanceToMiddle;

  print(manhattanDistance);
}