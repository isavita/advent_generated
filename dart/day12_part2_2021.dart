import 'dart:io';

void main() {
  final input = File('input.txt').readAsStringSync().trim();
  final answer = solve(input);
  print(answer);
}

int solve(String input) {
  final parsed = parseInput(input);

  final graph = <String, Map<String, bool>>{};
  for (final pair in parsed) {
    if (graph[pair[0]] == null) {
      graph[pair[0]] = <String, bool>{};
    }
    if (graph[pair[1]] == null) {
      graph[pair[1]] = <String, bool>{};
    }
    graph[pair[0]]![pair[1]] = true;
    graph[pair[1]]![pair[0]] = true;
  }

  return walk(graph, 'start', {'start': 5}, ['start'], false);
}

int walk(Map<String, Map<String, bool>> graph, String current, Map<String, int> visited, List<String> path, bool doubleUsed) {
  if (current == 'end') {
    return 1;
  }

  visited[current] = (visited[current] ?? 0) + 1;

  int pathsToEnd = 0;

  for (final visitable in graph[current]!.keys) {
    if (visitable == 'start') {
      continue;
    }

    if (visitable == visitable.toLowerCase() && (visited[visitable] ?? 0) > 0) {
      if (doubleUsed) {
        continue;
      } else {
        doubleUsed = true;
      }
    }

    path.add(visitable);
    pathsToEnd += walk(graph, visitable, visited, path, doubleUsed);
    path.removeLast();

    visited[visitable] = (visited[visitable] ?? 0) - 1;

    if (visitable == visitable.toLowerCase() && (visited[visitable] ?? 0) == 1) {
      doubleUsed = false;
    }
  }

  return pathsToEnd;
}

List<List<String>> parseInput(String input) {
  final ans = <List<String>>[];
  for (final line in input.split('\n')) {
    ans.add(line.split('-'));
  }
  return ans;
}