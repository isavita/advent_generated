
import 'dart:io';
import 'dart:convert';

class Move {
  final String label;
  final int x;
  final int y;

  Move(this.label, this.x, this.y);
}

void main() {
  final input = File('input.txt').readAsStringSync();
  final args = input.split('\n');
  final graph = args.where((line) => line.isNotEmpty).map((line) => line.split('')).toList();

  final H = graph.length;
  final W = graph[0].length;

  final moves = [
    Move('left', -1, 0),
    Move('up', 0, -1),
    Move('right', 1, 0),
    Move('down', 0, 1),
  ];

  int sum = 0;

  for (int y = 0; y < H; y++) {
    for (int x = 0; x < W; x++) {
      if (graph[y][x] == '.') {
        continue;
      }

      int area = 0;
      final target = graph[y][x];
      final visited = <String>{};
      final side = <String, Set<String>>{};

      void search(int cx, int cy, String label) {
        if (graph[cy][cx] != target) {
          if (label.isNotEmpty && !visited.contains('$cx,$cy')) {
            saveOuter(label, side, cx, cy);
          }
          return;
        }

        visited.add('$cx,$cy');
        area++;
        graph[cy][cx] = '.';

        for (final m in moves) {
          final nx = cx + m.x;
          final ny = cy + m.y;

          if (nx < 0 || nx >= W || ny < 0 || ny >= H) {
            saveOuter(m.label, side, nx, ny);
            continue;
          }
          search(nx, ny, m.label);
        }
      }

      search(x, y, '');
      final outer = countOuter(side);
      sum += area * outer;
    }
  }
  print(sum);
}

void saveOuter(String label, Map<String, Set<String>> side, int x, int y) {
  final key = (label == 'up' || label == 'down') ? '$y:$x' : '$x:$y';

  if (!side.containsKey(label)) {
    side[label] = {};
  }
  side[label]!.add(key);
}

int countOuter(Map<String, Set<String>> side) {
  int outer = 0;
  for (final label in side.keys) {
    final array = side[label]!.toList();
    array.sort((a, b) {
      final iParts = a.split(':').map(int.parse).toList();
      final jParts = b.split(':').map(int.parse).toList();
      if (iParts[0] == jParts[0]) {
        return iParts[1].compareTo(jParts[1]);
      }
      return iParts[0].compareTo(jParts[0]);
    });

    final temp = <String>[];
    for (final current in array) {
      final parts = current.split(':').map(int.parse).toList();
      if (!check(temp, parts[0], parts[1])) {
        outer++;
      }
      temp.add(current);
    }
  }
  return outer;
}

bool check(List<String> ary, int i, int j) {
  final search = ['$i:${j - 1}', '$i:${j + 1}'];
  for (final s in search) {
    if (ary.contains(s)) {
      return true;
    }
  }
  return false;
}
