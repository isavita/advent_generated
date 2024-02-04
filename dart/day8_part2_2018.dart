
import 'dart:io';

class Node {
  List<Node> children = [];
  List<int> metadata = [];
  
  int sumMetadata() {
    int sum = metadata.reduce((value, element) => value + element);
    for (var child in children) {
      sum += child.sumMetadata();
    }
    return sum;
  }
  
  int calculateValue() {
    if (children.isEmpty) {
      return metadata.reduce((value, element) => value + element);
    } else {
      int value = 0;
      for (var index in metadata) {
        if (index > 0 && index <= children.length) {
          value += children[index - 1].calculateValue();
        }
      }
      return value;
    }
  }
}

Node parseNode(List<int> data, int index) {
  Node node = Node();
  int numChildren = data[index];
  int numMetadata = data[index + 1];
  int i = index + 2;
  
  for (int j = 0; j < numChildren; j++) {
    Node child = parseNode(data, i);
    node.children.add(child);
    i += childSize(child);
  }
  
  for (int j = 0; j < numMetadata; j++) {
    node.metadata.add(data[i + j]);
  }
  
  return node;
}

int childSize(Node node) {
  int size = 2; // header size
  for (var child in node.children) {
    size += childSize(child);
  }
  size += node.metadata.length;
  return size;
}

void main() {
  List<int> data = File('input.txt').readAsStringSync().trim().split(' ').map(int.parse).toList();
  Node root = parseNode(data, 0);
  
  int sumMetadata = root.sumMetadata();
  int valueRoot = root.calculateValue();
  
  print(sumMetadata);
  print(valueRoot);
}
