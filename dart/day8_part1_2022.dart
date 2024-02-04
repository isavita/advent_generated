import 'dart:io';

void main() {
  var grid = {};
  var visible = {};
  var neighbors4 = [
    [0, 1],
    [0, -1],
    [1, 0],
    [-1, 0]
  ];

  var file = new File('input.txt');
  var lines = file.readAsLinesSync();
  var y = 0;

  for (var line in lines) {
    for (var x = 0; x < line.length; x++) {
      grid["$x,$y"] = int.parse(line[x]);
    }
    y++;
  }

  grid.keys.forEach((p) {
    for (var n in neighbors4) {
      var next = p.split(',').map(int.parse).toList();
      while (true) {
        next[0] += n[0];
        next[1] += n[1];
        var nextKey = "${next[0]},${next[1]}";
        if (grid.containsKey(nextKey)) {
          if (grid[nextKey] >= grid[p]) {
            break;
          }
        } else {
          visible[p] = true;
          break;
        }
      }
    }
  });

  print(visible.length);
}