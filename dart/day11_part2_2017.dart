import 'dart:io';

void main() {
  File file = File('input.txt');
  String path = file.readAsStringSync().trim();

  List<String> steps = path.split(',');

  int x = 0;
  int y = 0;
  int z = 0;
  int maxDistance = 0;

  for (String step in steps) {
    switch (step) {
      case 'n':
        y++;
        z--;
        break;
      case 'ne':
        x++;
        z--;
        break;
      case 'se':
        x++;
        y--;
        break;
      case 's':
        y--;
        z++;
        break;
      case 'sw':
        x--;
        z++;
        break;
      case 'nw':
        x--;
        y++;
        break;
    }

    int distance = (x.abs() + y.abs() + z.abs()) ~/ 2;
    if (distance > maxDistance) {
      maxDistance = distance;
    }
  }

  print(maxDistance);
}