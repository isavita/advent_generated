
import 'dart:io';
import 'dart:math';

void main() async {
  // Read weights from input.txt
  final weights = await readWeightsFromFile('input.txt');
  
  // Calculate total weight and target weight for each group
  final totalWeight = weights.reduce((a, b) => a + b);
  final targetWeight = totalWeight ~/ 4;

  if (totalWeight % 4 != 0) {
    print('Cannot balance the sleigh, total weight is not divisible by 4.');
    return;
  }

  // Find the optimal configuration
  final result = findOptimalGroup(weights, targetWeight);
  print('Quantum Entanglement of the first group in the ideal configuration: $result');
}

Future<List<int>> readWeightsFromFile(String filename) async {
  final file = File(filename);
  final contents = await file.readAsString();
  return contents.split('\n').map(int.parse).toList();
}

int findOptimalGroup(List<int> weights, int targetWeight) {
  int minCount = weights.length + 1;
  int minQE = 1 << 30; // A large number
  List<int> bestGroup = [];

  void backtrack(List<int> currentGroup, List<int> remaining, int currentWeight, int startIndex) {
    if (currentWeight > targetWeight) return;
    if (currentWeight == targetWeight) {
      if (currentGroup.length < minCount) {
        minCount = currentGroup.length;
        minQE = currentGroup.fold(1, (a, b) => a * b);
        bestGroup = List.from(currentGroup);
      } else if (currentGroup.length == minCount) {
        int currentQE = currentGroup.fold(1, (a, b) => a * b);
        if (currentQE < minQE) {
          minQE = currentQE;
          bestGroup = List.from(currentGroup);
        }
      }
      return;
    }

    for (int i = startIndex; i < remaining.length; i++) {
      backtrack(
        [...currentGroup, remaining[i]],
        remaining,
        currentWeight + remaining[i],
        i + 1,
      );
    }
  }

  backtrack([], weights, 0, 0);
  return minQE;
}
