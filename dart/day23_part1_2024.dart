
import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  final connections = <String, Set<String>>{};
  for (final line in lines) {
    final parts = line.split('-');
    connections.putIfAbsent(parts[0], () => {}).add(parts[1]);
    connections.putIfAbsent(parts[1], () => {}).add(parts[0]);
  }

  var count = 0;
  final computers = connections.keys.toList();
  for (var i = 0; i < computers.length; i++) {
    for (var j = i + 1; j < computers.length; j++) {
      for (var k = j + 1; k < computers.length; k++) {
        final c1 = computers[i];
        final c2 = computers[j];
        final c3 = computers[k];

        if (connections[c1]!.contains(c2) &&
            connections[c1]!.contains(c3) &&
            connections[c2]!.contains(c3)) {
          if (c1.startsWith('t') || c2.startsWith('t') || c3.startsWith('t')) {
            count++;
          }
        }
      }
    }
  }

  print(count);
}
