import 'dart:io';
import 'dart:math';

void main() {
  final x = [1];
  final lines = File('input.txt').readAsLinesSync();
  for (final line in lines) {
    switch (line) {
      case 'noop':
        x.add(x.last);
        break;
      default:
        final n = int.parse(line.split(' ')[1]);
        x.add(x.last);
        x.add(x.last + n);
        break;
    }
  }

  final grid = <Point<int>, void>{};
  for (var i = 0; i < x.length; i++) {
    final crtx = i % 40, crty = i ~/ 40;
    if ((crtx - x[i]).abs() <= 1) {
      grid[Point(crtx, crty)] = null;
    } else {
      grid.remove(Point(crtx, crty));
    }
  }

  for (var y = 0; y < 6; y++) {
    for (var x = 0; x < 40; x++) {
      stdout.write(grid.containsKey(Point(x, y)) ? '#' : '.');
    }
    print('');
  }
}