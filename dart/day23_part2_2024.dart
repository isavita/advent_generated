
import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();

  // Build an adjacency list representing the network connections
  final adjList = <String, Set<String>>{};
  for (final line in lines) {
    final parts = line.split('-');
    adjList.putIfAbsent(parts[0], () => {}).add(parts[1]);
    adjList.putIfAbsent(parts[1], () => {}).add(parts[0]);
  }

  // Part 1: Find sets of three inter-connected computers with at least one 't'
  int count = 0;
  final computers = adjList.keys.toList();
  for (int i = 0; i < computers.length; i++) {
    for (int j = i + 1; j < computers.length; j++) {
      for (int k = j + 1; k < computers.length; k++) {
        final c1 = computers[i];
        final c2 = computers[j];
        final c3 = computers[k];
        if (adjList[c1]!.contains(c2) &&
            adjList[c1]!.contains(c3) &&
            adjList[c2]!.contains(c3)) {
          if (c1.startsWith('t') || c2.startsWith('t') || c3.startsWith('t')) {
            count++;
          }
        }
      }
    }
  }
  print('Part 1: $count');

  // Part 2: Find the largest set of fully connected computers (clique)
  Set<String> largestClique = {};
  for (final computer in computers) {
    final currentClique = <String>{computer};
    for (final other in computers) {
      if (computer != other) {
        bool connectedToAll = true;
        for (final member in currentClique) {
          if (!adjList[other]!.contains(member)) {
            connectedToAll = false;
            break;
          }
        }
        if (connectedToAll) {
          currentClique.add(other);
        }
      }
    }

      bool isClique = true;
      for (final c1 in currentClique){
          for (final c2 in currentClique){
              if (c1 != c2 && !adjList[c1]!.contains(c2)){
                  isClique = false;
                  break;
              }
          }
          if (!isClique) break;
      }

    if (isClique && currentClique.length > largestClique.length) {
      largestClique = currentClique;
    }
  }

  // Generate the password
  final password = largestClique.toList()..sort();
  print('Part 2: ${password.join(',')}');
}
