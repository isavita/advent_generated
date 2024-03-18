import 'dart:io';
import 'dart:convert';

void dfs(int node, Map<int, List<int>> adj, Map<int, bool> visited) {
  visited[node] = true;
  for (var neighbor in adj[node] ?? []) {
    if (visited[neighbor] == false || visited[neighbor] == null) {
      dfs(neighbor, adj, visited);
    }
  }
}

void main() async {
  final file = File('input.txt');
  final lines = await file.readAsLines();

  // Adjacency list to store connections
  final adj = <int, List<int>>{};

  // Set to keep track of visited nodes
  final visited = <int, bool>{};

  int groups = 0;

  for (final line in lines) {
    final parts = line.split(' <-> ');
    final from = int.parse(parts[0]);
    final toNodes = parts[1].split(', ');

    for (final toNode in toNodes) {
      final to = int.parse(toNode);
      adj.putIfAbsent(from, () => []).add(to);
      adj.putIfAbsent(to, () => []).add(from);
      visited[from] = false;
      visited[to] = false;
    }
  }

  // Loop through all nodes to find connected components
  for (final node in adj.keys) {
    if (visited[node] == false || visited[node] == null) {
      dfs(node, adj, visited);
      groups++;
    }
  }

  print(groups);
}