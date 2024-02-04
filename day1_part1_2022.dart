import 'dart:io';

void main() {
  List<String> lines = File('input.txt').readAsLinesSync();
  
  List<int> calories = [];
  int maxCalories = 0;
  
  for (String line in lines) {
    if (line.isNotEmpty) {
      calories.add(int.parse(line));
    } else {
      int totalCalories = calories.reduce((value, element) => value + element);
      if (totalCalories > maxCalories) {
        maxCalories = totalCalories;
      }
      calories.clear();
    }
  }
  
  int totalCalories = calories.reduce((value, element) => value + element);
  if (totalCalories > maxCalories) {
    maxCalories = totalCalories;
  }
  
  print(maxCalories);
}