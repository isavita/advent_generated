
import 'dart:io';

void main() {
  var k = <int>[];
  var l = <int>[];
  var m = <int>[];

  var lines = File('input.txt').readAsLinesSync();
  for (var i = 0; i < lines.length; i++) {
    var line = lines[i];
    var v = 0;
    switch (i % 18) {
      case 4:
        v = int.parse(line.split(' ')[2]);
        l.add(v);
        break;
      case 5:
        v = int.parse(line.split(' ')[2]);
        k.add(v);
        break;
      case 15:
        v = int.parse(line.split(' ')[2]);
        m.add(v);
        break;
    }
  }

  var constraints = <int, List<int>>{};
  var stack = <int>[];
  for (var i = 0; i < l.length; i++) {
    switch (l[i]) {
      case 1:
        stack.add(i);
        break;
      case 26:
        var pop = stack.removeLast();
        constraints[pop] = [i, m[pop] + k[i]];
        break;
    }
  }

  var max = List<int>.filled(14, 0);
  for (var i = 0; i < 14; i++) {
    if (!constraints.containsKey(i)) {
      continue;
    }
    var vmax = 9;
    while (vmax + constraints[i]![1] > 9) {
      vmax--;
    }
    max[i] = vmax;
    max[constraints[i]![0]] = vmax + constraints[i]![1];
  }

  print(num(max));
}

int num(List<int> w) {
  var n = 0;
  for (var i = 0; i < w.length; i++) {
    n *= 10;
    n += w[i];
  }
  return n;
}
