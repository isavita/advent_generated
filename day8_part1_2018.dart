
import 'dart:io';

class Node {
  List<Node> children = [];
  List<int> metadata = [];
}

int sumMetadata(Node node) {
  int sum = 0;
  
  for (Node child in node.children) {
    sum += sumMetadata(child);
  }
  
  sum += node.metadata.fold(0, (a, b) => a + b);
  
  return sum;
}

Node parseNode(List<int> data, int index) {
  Node node = Node();
  
  int numChildren = data[index];
  int numMetadata = data[index + 1];
  int currentIndex = index + 2;
  
  for (int i = 0; i < numChildren; i++) {
    Node child = parseNode(data, currentIndex);
    node.children.add(child);
    currentIndex += childSize(child);
  }
  
  for (int i = 0; i < numMetadata; i++) {
    node.metadata.add(data[currentIndex + i]);
  }
  
  return node;
}

int childSize(Node node) {
  int size = 2; // Header size
  
  for (Node child in node.children) {
    size += childSize(child);
  }
  
  size += node.metadata.length;
  
  return size;
}

void main() {
  List<int> data = File('input.txt').readAsStringSync().trim().split(' ').map(int.parse).toList();
  
  Node root = parseNode(data, 0);
  
  int result = sumMetadata(root);
  
  print(result);
}
