import 'dart:io';

void main() {
  List<int> expenses = File('input.txt').readAsLinesSync().map(int.parse).toList();

  for (int i = 0; i < expenses.length; i++) {
    for (int j = i + 1; j < expenses.length; j++) {
      if (expenses[i] + expenses[j] == 2020) {
        print(expenses[i] * expenses[j]);
        return;
      }
    }
  }
}