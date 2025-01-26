
import 'dart:io';

void main() {
  final input = readInput('input.txt');
  final orderingRules = input[0];
  final updates = input[1];

  int sum = 0;
  for (var update in updates) {
    if (isCorrectlyOrdered(update, orderingRules)) {
      sum += update[update.length ~/ 2];
    }
  }

  print(sum);
}

List<List<List<int>>> readInput(String filename) {
  final file = File(filename);
  final lines = file.readAsLinesSync();

  final orderingRules = <List<int>>[];
  final updates = <List<int>>[];
  bool isUpdateSection = false;

  for (var line in lines) {
    line = line.trim();
    if (line.isEmpty) {
      isUpdateSection = true;
      continue;
    }

    if (!isUpdateSection) {
      final parts = line.split('|');
      if (parts.length != 2) continue;
      final x = int.tryParse(parts[0].trim());
      final y = int.tryParse(parts[1].trim());
      if (x == null || y == null) continue;
      orderingRules.add([x, y]);
    } else {
      final nums = line.split(',').map((numStr) => int.tryParse(numStr.trim())).whereType<int>().toList();
      if (nums.isNotEmpty) {
        updates.add(nums);
      }
    }
  }

  return [orderingRules, updates];
}

bool isCorrectlyOrdered(List<int> update, List<List<int>> rules) {
  final position = {for (var i = 0; i < update.length; i++) update[i]: i};

  for (var rule in rules) {
    final x = rule[0];
    final y = rule[1];
    final posX = position[x];
    final posY = position[y];
    if (posX != null && posY != null && posX >= posY) {
      return false;
    }
  }

  return true;
}
