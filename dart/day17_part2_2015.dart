import 'dart:io';

void main() {
  List<int> containers = File('input.txt').readAsLinesSync().map(int.parse).toList();
  int totalLiters = 150;

  int combinations = countCombinations(containers, totalLiters);
  print(combinations);

  int minContainers = findMinContainers(containers, totalLiters);
  int ways = countCombinationsWithMinContainers(containers, totalLiters, minContainers);
  print(ways);
}

int countCombinations(List<int> containers, int totalLiters) {
  int count = 0;
  for (int i = 1; i < (1 << containers.length); i++) {
    int sum = 0;
    for (int j = 0; j < containers.length; j++) {
      if ((i & (1 << j)) > 0) {
        sum += containers[j];
      }
    }
    if (sum == totalLiters) {
      count++;
    }
  }
  return count;
}

int findMinContainers(List<int> containers, int totalLiters) {
  int minContainers = containers.length;
  for (int i = 1; i < (1 << containers.length); i++) {
    int sum = 0;
    int count = 0;
    for (int j = 0; j < containers.length; j++) {
      if ((i & (1 << j)) > 0) {
        sum += containers[j];
        count++;
      }
    }
    if (sum == totalLiters && count < minContainers) {
      minContainers = count;
    }
  }
  return minContainers;
}

int countCombinationsWithMinContainers(List<int> containers, int totalLiters, int minContainers) {
  int count = 0;
  for (int i = 1; i < (1 << containers.length); i++) {
    int sum = 0;
    int countContainers = 0;
    for (int j = 0; j < containers.length; j++) {
      if ((i & (1 << j)) > 0) {
        sum += containers[j];
        countContainers++;
      }
    }
    if (sum == totalLiters && countContainers == minContainers) {
      count++;
    }
  }
  return count;
}