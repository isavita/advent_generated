
import 'dart:io';

void main() {
  List<int> calories = [];
  
  File file = new File('input.txt');
  List<String> lines = file.readAsLinesSync();
  
  int sum = 0;
  int max1 = 0;
  int max2 = 0;
  int max3 = 0;
  
  for (String line in lines) {
    if (line.isEmpty) {
      if (sum > max1) {
        max3 = max2;
        max2 = max1;
        max1 = sum;
      } else if (sum > max2) {
        max3 = max2;
        max2 = sum;
      } else if (sum > max3) {
        max3 = sum;
      }
      sum = 0;
    } else {
      sum += int.parse(line);
    }
  }
  
  int result = max1 + max2 + max3;
  print(result);
}
