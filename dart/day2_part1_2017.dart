
import 'dart:io';
import 'dart:convert';

void main() {
  final file = File('input.txt');
  final lines = file.readAsLinesSync();
  
  int checksum = 0;
  
  for (var line in lines) {
    final nums = line.split(RegExp(r'\s+')).map(int.parse).toList();
    final minVal = nums.reduce((a, b) => a < b ? a : b);
    final maxVal = nums.reduce((a, b) => a > b ? a : b);
    
    checksum += maxVal - minVal;
  }
  
  print(checksum);
}
