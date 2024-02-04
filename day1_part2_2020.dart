import 'dart:io';

void main() {
  List<int> expenses = [];
  
  File file = new File('input.txt');
  List<String> lines = file.readAsLinesSync();
  
  for (String line in lines) {
    expenses.add(int.parse(line));
  }
  
  for (int i = 0; i < expenses.length; i++) {
    for (int j = i + 1; j < expenses.length; j++) {
      if (expenses[i] + expenses[j] == 2020) {
        print(expenses[i] * expenses[j]);
      }
      for (int k = j + 1; k < expenses.length; k++) {
        if (expenses[i] + expenses[j] + expenses[k] == 2020) {
          print(expenses[i] * expenses[j] * expenses[k]);
        }
      }
    }
  }
}