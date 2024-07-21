
import 'dart:io';

void dfs(int node, Map<int, List<int>> adj, Set<int> visited) {
  visited.add(node);
  for (var neighbor in adj[node]!) {
    if (!visited.contains(neighbor)) {
      dfs(neighbor, adj, visited);
    }
  }
}

void main() async {
  var file = File('input.txt');
  var lines = await file.readAsLines();

  var adj = <int, List<int>>{};
  for (var line in lines) {
    var parts = line.split(' <-> ');
    var from = int.parse(parts[0]);
    var toNodes = parts[1].split(', ').map(int.parse);

    adj.putIfAbsent(from, () => []);
    for (var to in toNodes) {
      adj[from]!.add(to);
      adj.putIfAbsent(to, () => []).add(from);
    }
  }

  var visited = <int>{};
  dfs(0, adj, visited);

  print(visited.length);
}
