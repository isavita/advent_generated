import 'dart:io';

int abs(int x) => x < 0 ? -x : x;

int max(int a, int b) => a > b ? a : b;

int distance(int x, int y, int z) => (abs(x) + abs(y) + abs(z)) ~/ 2;

void main() {
  var file = File('input.txt');
  var input = file.readAsStringSync().trim();

  var directions = input.split(',');

  var x = 0, y = 0, z = 0;
  var maxDistance = 0;

  for (var dir in directions) {
    switch (dir) {
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

    var curDistance = distance(x, y, z);
    maxDistance = max(maxDistance, curDistance);
  }

  print(distance(x, y, z));
}