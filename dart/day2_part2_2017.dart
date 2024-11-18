
import 'dart:io';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  
  int sum = 0;
  
  for (final line in lines) {
    final nums = line.split(RegExp(r'\s+')).map(int.parse).toList();
    
    for (int i = 0; i < nums.length; i++) {
      for (int j = 0; j < nums.length; j++) {
        if (i != j && nums[i] % nums[j] == 0) {
          sum += nums[i] ~/ nums[j];
        }
      }
    }
  }
  
  print(sum);
}
