
import 'dart:io';
import 'dart:collection';

void main() {
  List<String> input = File('input.txt').readAsLinesSync();
  print(solve(input));
}

int solve(List<String> input) {
  const minCut = 3;
  Map<String, Set<String>> graph = parseInput(input);

  String source = graph.keys.first;

  late Map<String, Set<String>> separatedGraph;
  for (String end in graph.keys) {
    if (source == end) continue;

    Map<String, Set<String>> newGraph = copyGraph(graph);
    for (int i = 0; i < minCut; i++) {
      List<String> path = findShortestPath(newGraph, source, end);
      for (int j = 0; j < path.length - 1; j++) {
        newGraph[path[j]]?.remove(path[j + 1]);
        newGraph[path[j + 1]]?.remove(path[j]);
      }
    }

    if (!canReach(newGraph, source, end)) {
      separatedGraph = newGraph;
      break;
    }
  }

  Set<String> reachable = bfs(separatedGraph, source);
  int length1 = reachable.length;
  int length2 = separatedGraph.length - length1;

  return length1 * length2;
}

Map<String, Set<String>> parseInput(List<String> input) {
  Map<String, Set<String>> graph = {};

  for (String line in input) {
    List<String> parts = line.split(': ');
    String vertex = parts[0];
    List<String> others = parts[1].split(' ');

    graph.putIfAbsent(vertex, () => {});
    for (String other in others) {
      graph.putIfAbsent(other, () => {});
      graph[vertex]!.add(other);
      graph[other]!.add(vertex);
    }
  }

  return graph;
}

List<String> findShortestPath(Map<String, Set<String>> graph, String start, String end) {
  Queue<String> queue = Queue();
  Map<String, String?> cameFrom = {start: null};
  queue.add(start);

  while (queue.isNotEmpty) {
    String current = queue.removeFirst();
    if (current == end) break;

    for (String neighbor in graph[current] ?? []) {
      if (!cameFrom.containsKey(neighbor)) {
        queue.add(neighbor);
        cameFrom[neighbor] = current;
      }
    }
  }

  return reconstructPath(start, end, cameFrom);
}

List<String> reconstructPath(String start, String end, Map<String, String?> cameFrom) {
  List<String> path = [end];
  String current = end;
  while (current != start) {
    current = cameFrom[current]!;
    path.insert(0, current);
  }
  return path;
}

bool canReach(Map<String, Set<String>> graph, String start, String end) {
  return bfs(graph, start).contains(end);
}

Set<String> bfs(Map<String, Set<String>> graph, String start) {
  Set<String> reachable = {};
  Queue<String> queue = Queue();
  queue.add(start);
  reachable.add(start);

  while (queue.isNotEmpty) {
    String current = queue.removeFirst();
    for (String neighbor in graph[current] ?? []) {
      if (!reachable.contains(neighbor)) {
        queue.add(neighbor);
        reachable.add(neighbor);
      }
    }
  }

  return reachable;
}

Map<String, Set<String>> copyGraph(Map<String, Set<String>> graph) {
  return {
    for (var entry in graph.entries)
      entry.key: Set.from(entry.value)
  };
}
