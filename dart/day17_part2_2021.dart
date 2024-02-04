import 'dart:io';

void main() {
  var file = new File('input.txt');
  var lines = file.readAsLinesSync();
  var parts = lines[0].split(', ');
  var xRange = parts[0].substring(15).split('..');
  var yRange = parts[1].substring(2).split('..');
  var xMin = int.parse(xRange[0]);
  var xMax = int.parse(xRange[1]);
  var yMin = int.parse(yRange[0]);
  var yMax = int.parse(yRange[1]);

  var velocities = <String, bool>{};
  for (var xVel = -1000; xVel <= 1000; xVel++) {
    for (var yVel = -1000; yVel <= 1000; yVel++) {
      var xPos = 0;
      var yPos = 0;
      var curXVel = xVel;
      var curYVel = yVel;
      var inTargetArea = false;
      while (true) {
        xPos += curXVel;
        yPos += curYVel;

        if (xPos >= xMin && xPos <= xMax && yPos >= yMin && yPos <= yMax) {
          inTargetArea = true;
          break;
        }

        if (isMovingAway(xPos, yPos, curXVel, curYVel, xMin, xMax, yMin, yMax)) {
          break;
        }

        if (curXVel > 0) {
          curXVel--;
        } else if (curXVel < 0) {
          curXVel++;
        }

        curYVel--;
      }

      if (inTargetArea) {
        var velocityKey = '$xVel,$yVel';
        velocities[velocityKey] = true;
      }
    }
  }

  print(velocities.length);
}

bool isMovingAway(int xPos, int yPos, int xVel, int yVel, int xMin, int xMax, int yMin, int yMax) {
  if (xPos < xMin && xVel < 0) {
    return true;
  }
  if (xPos > xMax && xVel > 0) {
    return true;
  }
  if (yPos < yMin && yVel < 0) {
    return true;
  }
  return false;
}