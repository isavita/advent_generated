
import 'dart:io';

void main() {
  List<int> modules = File('input.txt').readAsLinesSync().map(int.parse).toList();
  
  int totalFuel = modules.map((mass) => (mass ~/ 3) - 2).reduce((a, b) => a + b);
  
  print(totalFuel);
}
