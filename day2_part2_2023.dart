import 'dart:io';
import 'dart:convert';

void main() {
  File file = File('input.txt');
  List<String> lines = file.readAsLinesSync();
  RegExp gameRegex = RegExp(r'Game (\d+): (.+)');
  RegExp cubeRegex = RegExp(r'(\d+) (red|green|blue)');
  int totalPower = 0;

  for (String line in lines) {
    Match? matches = gameRegex.firstMatch(line);

    if (matches != null) {
      List<String> rounds = matches.group(2)!.split(';');
      int maxRed = 0, maxGreen = 0, maxBlue = 0;

      for (String round in rounds) {
        Iterable<Match> cubes = cubeRegex.allMatches(round);
        int red = 0, green = 0, blue = 0;

        for (Match cube in cubes) {
          int count = int.parse(cube.group(1)!);
          switch (cube.group(2)) {
            case 'red':
              red += count;
              break;
            case 'green':
              green += count;
              break;
            case 'blue':
              blue += count;
              break;
          }
        }

        if (red > maxRed) {
          maxRed = red;
        }
        if (green > maxGreen) {
          maxGreen = green;
        }
        if (blue > maxBlue) {
          maxBlue = blue;
        }
      }

      int power = maxRed * maxGreen * maxBlue;
      totalPower += power;
    }
  }

  print(totalPower);
}