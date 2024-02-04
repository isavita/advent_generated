import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> components = lines.map((line) => line.split('/').map(int.parse).toList()).toList();

  int maxStrength = findMaxStrength(components, 0, 0);
  print(maxStrength);
}

int findMaxStrength(List<List<int>> components, int port, int usedIndex) {
  int maxStrength = 0;

  for (int i = 0; i < components.length; i++) {
    if (components[i][0] == port || components[i][1] == port) {
      List<List<int>> newComponents = List.from(components);
      newComponents.removeAt(i);

      int nextPort = components[i][0] == port ? components[i][1] : components[i][0];
      int strength = port + nextPort + findMaxStrength(newComponents, nextPort, i);

      if (strength > maxStrength) {
        maxStrength = strength;
      }
    }
  }

  return maxStrength;
}