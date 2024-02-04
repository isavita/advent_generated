
import 'dart:io';

void main() {
  var k = <int>[];
  var l = <int>[];
  var m = <int>[];

  var lines = File('input.txt').readAsLinesSync();

  var i = 0;
  for (var line in lines) {
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
    i++;
  }

  var constraints = <int, List<int>>{};
  var stack = <int>[];
  for (var i = 0; i < l.length; i++) {
    if (l[i] == 1) {
      stack.add(i);
    } else if (l[i] == 26) {
      var pop = stack.removeLast();
      constraints[pop] = [i, m[pop] + k[i]];
    }
  }

  var min = List<int>.filled(14, 0);
  for (var i = 0; i < 14; i++) {
    if (!constraints.containsKey(i)) {
      continue;
    }
    var vmin = 1;
    while (vmin + constraints[i]![1] < 1) {
      vmin++;
    }
    min[i] = vmin;
    min[constraints[i]![0]] = vmin + constraints[i]![1];
  }

  print(num(min));
}

int num(List<int> w) {
  var n = 0;
  for (var i = 0; i < w.length; i++) {
    n *= 10;
    n += w[i];
  }
  return n;
}
