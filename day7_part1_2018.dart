
import 'dart:io';

void main() {
  var lines = File('input.txt').readAsLinesSync();
  var deps = <String, List<String>>{};
  var allSteps = <String>{};

  for (var line in lines) {
    var a = line[5];
    var b = line[36];
    deps.putIfAbsent(b, () => []).add(a);
    allSteps.addAll([a, b]);
  }

  var order = topologicalSort(deps, allSteps);
  print(order);
}

String topologicalSort(Map<String, List<String>> deps, Set<String> allSteps) {
  var order = <String>[];
  var available = <String>[];

  for (var step in allSteps) {
    if (deps[step] == null || deps[step]!.isEmpty) {
      available.add(step);
    }
  }
  available.sort();

  while (available.isNotEmpty) {
    var next = available.removeAt(0);
    order.add(next);

    for (var step in allSteps) {
      if (deps[step]?.contains(next) ?? false) {
        deps[step]!.remove(next);
        if (deps[step]!.isEmpty) {
          available.add(step);
        }
      }
    }
    available.sort();
  }
  return order.join();
}
