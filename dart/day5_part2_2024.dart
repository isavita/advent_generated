
import 'dart:io';
import 'dart:convert';

void main() {
  final orderingRules = <List<int>>[];
  final updates = <List<int>>[];
  bool isUpdateSection = false;

  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  for (final line in lines) {
    final trimmedLine = line.trim();
    if (trimmedLine.isEmpty) {
      isUpdateSection = true;
      continue;
    }

    if (!isUpdateSection) {
      final parts = trimmedLine.split('|');
      if (parts.length != 2) continue;
      final x = int.tryParse(parts[0].trim());
      final y = int.tryParse(parts[1].trim());
      if (x == null || y == null) continue;
      orderingRules.add([x, y]);
    } else {
      final nums = trimmedLine.split(',').map((numStr) => int.tryParse(numStr.trim())).whereType<int>().toList();
      if (nums.isNotEmpty) {
        updates.add(nums);
      }
    }
  }

  int sum = 0;
  for (final update in updates) {
    if (!isCorrectlyOrdered(update, orderingRules)) {
      final sortedUpdate = sortUpdate(update, orderingRules);
      final middlePage = sortedUpdate[sortedUpdate.length ~/ 2];
      sum += middlePage;
    }
  }

  print(sum);
}

bool isCorrectlyOrdered(List<int> update, List<List<int>> rules) {
  final position = <int, int>{};
  for (int idx = 0; idx < update.length; idx++) {
    position[update[idx]] = idx;
  }

  for (final rule in rules) {
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

List<int> sortUpdate(List<int> update, List<List<int>> rules) {
  final adjacency = <int, List<int>>{};
  final pagesInUpdate = <int>{};
  for (final page in update) {
    pagesInUpdate.add(page);
    adjacency[page] = [];
  }

  for (final rule in rules) {
    final x = rule[0];
    final y = rule[1];
    if (pagesInUpdate.contains(x) && pagesInUpdate.contains(y)) {
      adjacency[x]!.add(y);
    }
  }

  final visited = <int>{};
  final tempMarked = <int>{};
  final result = <int>[];

  void visit(int n) {
    if (tempMarked.contains(n)) {
      throw Exception('Cycle detected');
    }
    if (!visited.contains(n)) {
      tempMarked.add(n);
      for (final m in adjacency[n]!) {
        visit(m);
      }
      tempMarked.remove(n);
      visited.add(n);
      result.add(n);
    }
  }

  for (final page in pagesInUpdate) {
    if (!visited.contains(page)) {
      visit(page);
    }
  }

  result.reversed.toList();
  return result;
}
