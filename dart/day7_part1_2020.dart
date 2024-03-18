import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  final contains = <String, List<String>>{};

  for (final line in lines) {
    final parts = line.split(' bags contain ');
    final container = parts[0];
    if (parts[1] == 'no other bags.') continue;
    final containedBags = parts[1].split(', ');
    for (final bag in containedBags) {
      final bagName = bag.split(' ').sublist(1, 3).join(' ');
      contains.putIfAbsent(bagName, () => []).add(container);
    }
  }

  final count = countCanContain('shiny gold', contains);
  print(count);
}

int countCanContain(String target, Map<String, List<String>> contains) {
  final seen = <String, bool>{};
  void dfs(String bag) {
    for (final outer in contains[bag] ?? []) {
      if (!seen.containsKey(outer)) {
        seen[outer] = true;
        dfs(outer);
      }
    }
  }

  dfs(target);
  return seen.length;
}