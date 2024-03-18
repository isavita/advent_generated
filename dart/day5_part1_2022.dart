import 'dart:io';
import 'dart:collection';

void main() {
  final input = File('input.txt').readAsStringSync().trim().split('\n\n');
  final stacks = _buildStacks(input[0].split('\n'));
  final steps = input[1].split('\n');
  print(_move(stacks, steps));
}

List<Queue<String>> _buildStacks(List<String> lines) {
  final stacks = List.generate((lines[0].length + 1) ~/ 4, (_) => Queue<String>());
  for (var i = lines.length - 1; i >= 0; i--) {
    for (var j = 1, k = 0; j < lines[i].length; j += 4, k++) {
      if (lines[i][j] != ' ') {
        stacks[k].addLast(lines[i][j]);
      }
    }
  }
  return stacks;
}

String _move(List<Queue<String>> stacks, List<String> steps) {
  for (final step in steps) {
    final parts = step.split(' ');
    final count = int.parse(parts[1]);
    final from = int.parse(parts[3]) - 1;
    final to = int.parse(parts[5]) - 1;
    final temp = Queue<String>();
    for (var i = 0; i < count; i++) {
      temp.addLast(stacks[from].removeLast());
    }
    stacks[to].addAll(temp);
  }
  return stacks.map((stack) => stack.isNotEmpty ? stack.last : '').join();
}