import 'dart:io';

class Marble {
  int value;
  Marble? prev;
  Marble? next;

  Marble(this.value);
}

void main() {
  final inputList = readInput('input.txt');
  final players = inputList[0];
  final lastMarble = inputList[1];
  // Multiply last marble number by 100
  final maxMarble = lastMarble * 100;
  print(playMarbleGame(players, maxMarble));
}

List<int> readInput(String filename) {
  final file = File(filename).readAsLinesSync();
  final parts = file[0].split(' ');
  return [int.parse(parts[0]), int.parse(parts[6])];
}

int playMarbleGame(int players, int lastMarble) {
  final scores = List.filled(players, 0);
  var current = Marble(0);
  current.next = current;
  current.prev = current;

  for (var marble = 1; marble <= lastMarble; marble++) {
    if (marble % 23 == 0) {
      final player = (marble - 1) % players;
      for (var i = 0; i < 7; i++) {
        current = current.prev!;
      }
      scores[player] += marble + current.value;
      current.prev!.next = current.next;
      current.next!.prev = current.prev;
      current = current.next!;
    } else {
      current = current.next!;
      final newMarble = Marble(marble)
        ..prev = current
        ..next = current.next;
      current.next!.prev = newMarble;
      current.next = newMarble;
      current = newMarble;
    }
  }

  return scores.reduce((max, score) => score > max ? score : max);
}