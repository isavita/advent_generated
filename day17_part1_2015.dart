
import 'dart:io';

void main() {
  List<int> containers = File('input.txt').readAsLinesSync().map(int.parse).toList();
  int totalLiters = 150;
  
  int count = countCombinations(containers, totalLiters, 0, 0);
  
  print(count);
}

int countCombinations(List<int> containers, int totalLiters, int index, int currentLiters) {
  if (currentLiters == totalLiters) {
    return 1;
  }
  
  if (index >= containers.length || currentLiters > totalLiters) {
    return 0;
  }
  
  return countCombinations(containers, totalLiters, index + 1, currentLiters) +
         countCombinations(containers, totalLiters, index + 1, currentLiters + containers[index]);
}
