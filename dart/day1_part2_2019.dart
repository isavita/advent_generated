import 'dart:io';

void main() {
  List<int> modules = File('input.txt').readAsLinesSync().map(int.parse).toList();
  
  int totalFuel = 0;
  
  for (int mass in modules) {
    int fuel = mass ~/ 3 - 2;
    totalFuel += fuel;
    
    while (fuel > 0) {
      fuel = fuel ~/ 3 - 2;
      if (fuel > 0) {
        totalFuel += fuel;
      }
    }
  }
  
  print(totalFuel);
}