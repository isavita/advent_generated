
import 'dart:io';

Map<String, List<String>> adj = {};
Map<String, int> memo = {};

int countPaths(String current, String target) {
  if (current == target) return 1;
  final key = '$current->$target';
  if (memo.containsKey(key)) return memo[key]!;
  int count = 0;
  final neighbors = adj[current];
  if (neighbors != null) {
    for (final next in neighbors) {
      count += countPaths(next, target);
    }
  }
  memo[key] = count;
  return count;
}

void main() {
  final file = File('input.txt');
  if (!file.existsSync()) return;
  for (final line in file.readAsLinesSync()) {
    final trimmed = line.trim();
    if (trimmed.isEmpty || !trimmed.contains(':')) continue;
    final parts = trimmed.split(':');
    final node = parts[0].trim();
    final targets = parts[1].trim().split(RegExp(r'\s+'));
    adj.putIfAbsent(node, () => []).addAll(targets);
  }
  final pathOrder1 = countPaths('svr', 'dac') * countPaths('dac', 'fft') * countPaths('fft', 'out');
  final pathOrder2 = countPaths('svr', 'fft') * countPaths('fft', 'dac') * countPaths('dac', 'out');
  print(pathOrder1 + pathOrder2);
}
