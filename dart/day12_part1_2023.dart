
import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  int totalArrangements = 0;
  
  for (String line in lines) {
    List<String> parts = line.split(' ');
    String row = parts[0];
    List<int> groups = parts[1].split(',').map(int.parse).toList();
    
    List<int> unknownIndices = [];
    for (int i = 0; i < row.length; i++) {
      if (row[i] == '?') {
        unknownIndices.add(i);
      }
    }
    
    int count = countArrangements(row, groups, unknownIndices, 0);
    totalArrangements += count;
  }
  
  print(totalArrangements);
}

int countArrangements(String row, List<int> groups, List<int> unknownIndices, int index) {
  if (index == unknownIndices.length) {
    return isValidArrangement(row, groups) ? 1 : 0;
  }
  
  int count = 0;
  for (int i = 0; i < 2; i++) {
    row = row.replaceRange(unknownIndices[index], unknownIndices[index] + 1, i == 0 ? '.' : '#');
    count += countArrangements(row, groups, unknownIndices, index + 1);
  }
  
  return count;
}

bool isValidArrangement(String row, List<int> groups) {
  List<int> groupSizes = [];
  int count = 0;
  
  for (int i = 0; i < row.length; i++) {
    if (row[i] == '#') {
      count++;
    } else if (count > 0) {
      groupSizes.add(count);
      count = 0;
    }
  }
  
  if (count > 0) {
    groupSizes.add(count);
  }
  
  if (groupSizes.length != groups.length) {
    return false;
  }
  
  for (int i = 0; i < groupSizes.length; i++) {
    if (groupSizes[i] != groups[i]) {
      return false;
    }
  }
  
  return true;
}
