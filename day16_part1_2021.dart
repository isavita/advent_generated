
import 'dart:io';

void main() {
  String input = File('input.txt').readAsStringSync();
  int sum = 0;
  
  for (int i = 0; i < input.length; i += 2) {
    String hex = input.substring(i, i + 2);
    int decimal = int.parse(hex, radix: 16);
    String binary = decimal.toRadixString(2).padLeft(8, '0');
    
    int version = int.parse(binary.substring(0, 3), radix: 2);
    
    sum += version;
  }
  
  print(sum);
}
