import 'dart:io';
import 'dart:convert';

void main() {
  var file = File('input.txt');
  var totalSum = 0;

  file.openRead().transform(utf8.decoder).transform(LineSplitter()).forEach((line) {
    var regex = RegExp(r'Game (\d+): (.+)');
    var cubeRegex = RegExp(r'(\d+) (red|green|blue)');

    var matches = regex.firstMatch(line);

    if (matches != null) {
      var gameId = int.parse(matches.group(1)!);
      var rounds = matches.group(2)!.split(';');
      var isValid = true;

      for (var round in rounds) {
        var cubes = cubeRegex.allMatches(round);
        var red = 0, green = 0, blue = 0;

        for (var cube in cubes) {
          var count = int.parse(cube.group(1)!);
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

          if (red > 12 || green > 13 || blue > 14) {
            isValid = false;
            break;
          }
        }

        if (!isValid) {
          break;
        }
      }

      if (isValid) {
        totalSum += gameId;
      }
    }
  }).then((_) {
    print(totalSum);
  }).catchError((e) {
    print('Error reading file: $e');
  });
}