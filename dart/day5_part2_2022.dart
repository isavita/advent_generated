import 'dart:io';

void main() {
  var s = File('input.txt').readAsStringSync().split('\n\n');
  var input = s[0].split('\n');
  var stacks = List<List<int>>.generate((input[0].length + 1) ~/ 4, (index) => []);

  for (var line in input) {
    for (var i = 0; i < line.length; i++) {
      var b = line.codeUnitAt(i);
      if (b >= 65 && b <= 90) {
        stacks[(i - 1) ~/ 4].add(b);
      }
    }
  }

  var steps = s[1].split('\n');
  print(move(stacks, steps));
}

String move(List<List<int>> st, List<String> steps) {
  var stacks = List<List<int>>.generate(st.length, (index) => []);

  for (var i = 0; i < st.length; i++) {
    stacks[i] = List<int>.generate(st[i].length, (index) => st[i][st[i].length - index - 1]);
  }

  for (var step in steps) {
    var parts = step.split(' ');
    var n = int.parse(parts[1]);
    var from = int.parse(parts[3]) - 1;
    var to = int.parse(parts[5]) - 1;

    stacks[to].addAll(stacks[from].sublist(stacks[from].length - n));
    stacks[from].removeRange(stacks[from].length - n, stacks[from].length);
  }

  var b = List<int>.generate(stacks.length, (index) => stacks[index][stacks[index].length - 1]);
  return String.fromCharCodes(b);
}