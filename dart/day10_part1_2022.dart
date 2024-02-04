
import 'dart:io';

void main() {
  List<int> x = [1];
  List<String> lines = File('input.txt').readAsLinesSync();
  
  for (String line in lines) {
    switch (line) {
      case 'noop':
        x.add(x[x.length - 1]);
        break;
      default:
        int n = int.parse(line.substring(5));
        x.add(x[x.length - 1]);
        x.add(x[x.length - 1] + n);
        break;
    }
  }
  
  int sum = 0;
  for (int i = 0; i < x.length; i++) {
    if ((i - 19) % 40 == 0) {
      sum += (i + 1) * x[i];
    }
  }
  
  print(sum);
}
