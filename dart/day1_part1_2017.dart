
import 'dart:io';

void main() {
  var input = File('input.txt').readAsStringSync().trim();
  var sum = 0;
  
  for (int i = 0; i < input.length; i++) {
    if (input[i] == input[(i + 1) % input.length]) {
      sum += int.parse(input[i]);
    }
  }
  
  print(sum);
}
