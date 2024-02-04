
import 'dart:io';

void main() {
  List<int> positions = File('input.txt').readAsStringSync().trim().split(',').map(int.parse).toList();
  positions.sort();
  int median = positions[positions.length ~/ 2];
  int totalFuel = 0;
  
  for (int pos in positions) {
    totalFuel += (pos - median).abs();
  }
  
  print(totalFuel);
}
