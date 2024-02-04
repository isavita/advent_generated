
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  List<List<int>> components = lines.map((line) => line.split('/').map(int.parse).toList()).toList();

  int maxStrength = 0;
  int maxLength = 0;
  int maxStrengthLongest = 0;

  void buildBridge(List<List<int>> remainingComponents, int currentPort, int currentStrength, int currentLength) {
    maxStrength = currentStrength > maxStrength ? currentStrength : maxStrength;
    if (currentLength > maxLength) {
      maxLength = currentLength;
      maxStrengthLongest = currentStrength;
    } else if (currentLength == maxLength) {
      maxStrengthLongest = currentStrength > maxStrengthLongest ? currentStrength : maxStrengthLongest;
    }

    for (int i = 0; i < remainingComponents.length; i++) {
      if (remainingComponents[i][0] == currentPort || remainingComponents[i][1] == currentPort) {
        int nextPort = remainingComponents[i][0] == currentPort ? remainingComponents[i][1] : remainingComponents[i][0];
        List<List<int>> newRemainingComponents = List.from(remainingComponents);
        newRemainingComponents.removeAt(i);
        buildBridge(newRemainingComponents, nextPort, currentStrength + currentPort + nextPort, currentLength + 1);
      }
    }
  }

  buildBridge(components, 0, 0, 0);

  print(maxStrength);
  print(maxStrengthLongest);
}
