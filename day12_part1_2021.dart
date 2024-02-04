import 'dart:io';

class Cave {
  Map<String, bool> connections = {};

  void connectTo(String name) {
    connections[name] = true;
  }

  void disconnectFrom(String name) {
    connections.remove(name);
  }
}

void main() {
  var caves = <String, Cave>{};
  var file = File('input.txt');
  var lines = file.readAsLinesSync();

  for (var line in lines) {
    var paths = line.split('-');
    var from = paths[0];
    var to = paths[1];

    if (!caves.containsKey(from)) {
      caves[from] = Cave();
    }

    if (!caves.containsKey(to)) {
      caves[to] = Cave();
    }

    caves[from]!.connectTo(to);
    caves[to]!.connectTo(from);
  }

  var count = 0;
  void dfs(String current, Map<String, bool> visited) {
    if (current == 'end') {
      count++;
      return;
    }

    for (var next in caves[current]!.connections.keys) {
      if (visited[next] == true && next.toLowerCase() == next) {
        continue;
      }

      var visitedCopy = Map<String, bool>.from(visited);
      visitedCopy[next] = true;
      dfs(next, visitedCopy);
    }
  }

  dfs('start', {'start': true});
  print(count);
}