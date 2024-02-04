
import 'dart:io';

void main() {
  List<int> numbers = File('input.txt').readAsStringSync().trim().split('').map(int.parse).toList();
  int sum = 0;
  
  for (int i = 0; i < numbers.length; i++) {
    if (numbers[i] == numbers[(i + numbers.length ~/ 2) % numbers.length]) {
      sum += numbers[i];
    }
  }
  
  print(sum);
}
