
import 'dart:io';

void main() {
  final input = File('input.txt').readAsLinesSync();
  final deps = <String, List<String>>{};
  final allSteps = <String, int>{};

  for (final line in input) {
    final a = line[5];
    final b = line[36];
    deps.putIfAbsent(b, () => []).add(a);
    allSteps.putIfAbsent(a, () => a.codeUnitAt(0) - 64 + 60);
    allSteps.putIfAbsent(b, () => b.codeUnitAt(0) - 64 + 60);
  }

  final numWorkers = 5;
  final workers = List.filled(numWorkers, 0);
  final tasks = List.filled(numWorkers, '');
  var time = 0;

  while (allSteps.isNotEmpty) {
    final available = allSteps.keys
        .where((step) => !deps.containsKey(step) || deps[step]!.isEmpty)
        .where((step) => !tasks.contains(step))
        .toList()..sort();

    for (var i = 0; i < numWorkers; i++) {
      if (workers[i] == 0 && available.isNotEmpty) {
        tasks[i] = available.first;
        workers[i] = allSteps[available.first]!;
        available.removeAt(0);
      }
    }

    final minDuration = workers.where((d) => d > 0).reduce((a, b) => a < b ? a : b);
    for (var i = 0; i < numWorkers; i++) {
      if (workers[i] > 0) {
        workers[i] -= minDuration;
        if (workers[i] == 0) {
          final completedTask = tasks[i];
          deps.remove(completedTask);
          for (final step in allSteps.keys) {
            deps[step]?.remove(completedTask);
          }
          allSteps.remove(completedTask);
          tasks[i] = '';
        }
      }
    }
    time += minDuration;
  }

  print(time);
}
