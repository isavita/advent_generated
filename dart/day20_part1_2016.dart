
import 'dart:io';

void main() {
  List<List<int>> blockedIPs = [];
  
  File file = new File('input.txt');
  List<String> lines = file.readAsLinesSync();
  
  for (String line in lines) {
    List<int> range = line.split('-').map(int.parse).toList();
    blockedIPs.add([range[0], range[1]]);
  }
  
  blockedIPs.sort((a, b) => a[0].compareTo(b[0]));
  
  int lowestIP = 0;
  
  for (List<int> range in blockedIPs) {
    if (lowestIP < range[0]) {
      break;
    }
    lowestIP = range[1] + 1;
  }
  
  print(lowestIP);
}
