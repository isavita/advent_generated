import 'dart:io';
import 'dart:convert';

class Node {
  int used;
  int avail;

  Node(this.used, this.avail);
}

void main() {
  List<Node> nodes = readNodes("input.txt");
  int viablePairs = countViablePairs(nodes);
  print(viablePairs);
}

List<Node> readNodes(String filename) {
  List<Node> nodes = [];
  File file = File(filename);
  List<String> lines = file.readAsLinesSync();

  RegExp nodeRegex = RegExp(r'node-x\d+-y\d+\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%');
  for (String line in lines) {
    RegExpMatch? matches = nodeRegex.firstMatch(line);
    if (matches != null) {
      int used = int.parse(matches.group(1)!);
      int avail = int.parse(matches.group(2)!);
      nodes.add(Node(used, avail));
    }
  }
  return nodes;
}

int countViablePairs(List<Node> nodes) {
  int count = 0;
  for (int i = 0; i < nodes.length; i++) {
    for (int j = 0; j < nodes.length; j++) {
      if (i != j && nodes[i].used > 0 && nodes[i].used <= nodes[j].avail) {
        count++;
      }
    }
  }
  return count;
}